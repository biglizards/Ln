pub mod ct {
    use crate::Value::{Bool, Int};
    use crate::{LinearStore, Location, Store, Value};
    use std::marker::PhantomData;
    use parser::L1;

    trait IsValue {}
    impl IsValue for i64 {}
    impl IsValue for bool {}

    #[derive(Clone, Copy, Debug)]
    struct TypedLocation<T: IsValue> {
        loc: Location,
        phantom: PhantomData<T>,
    }

    trait Step<S: Store, T> {
        fn step(&self, store: &mut S) -> T;
    }

    impl<S: Store, T: Copy> Step<S, T> for T {
        fn step(&self, _store: &mut S) -> T {
            *self
        }
    }

    impl<S: Store> Step<S, Value> for Value {
        fn step(&self, store: &mut S) -> Value {
            self.clone()
        }
    }

    impl<S: Store> Step<S, bool> for Value {
        fn step(&self, store: &mut S) -> bool {
            if let Bool(b) = self {
                *b
            } else {
                panic!(":(")
            }
        }
    }
    impl<S: Store> Step<S, i64> for Value {
        fn step(&self, store: &mut S) -> i64 {
            if let Int(b) = self {
                *b
            } else {
                panic!(":(")
            }
        }
    }
    impl<S: Store> Step<S, Value> for bool {
        fn step(&self, store: &mut S) -> Value {
            Bool(*self)
        }
    }
    impl<S: Store> Step<S, Value> for i64 {
        fn step(&self, store: &mut S) -> Value {
            Int(*self)
        }
    }

    #[derive(Debug)]
    struct Add<E1, E2> {
    e1: E1,
    e2: E2,
}
    impl<S: Store, E1: Step<S, i64>, E2: Step<S, i64>> Step<S, i64> for Add<E1, E2> {
        fn step(&self, store: &mut S) -> i64 {
            self.e1.step(store) + self.e2.step(store)
        }
    }
    // impl<S: Store, T, E1: Step<S, i64>, E2: Step<S, i64>> Step<S, T> for Add<E1, E2>
    // where
    //     i64: Step<S, T>,
    // {
    //     fn step(&self, store: &mut S) -> T {
    //         (self.e1.step(store) + self.e2.step(store)).step(store)
    //     }
    // }

    #[derive(Debug)]
    struct GE<E1, E2> {
    e1: E1,
    e2: E2,
}
    impl<S: Store, E1: Step<S, i64>, E2: Step<S, i64>> Step<S, bool> for GE<E1, E2> {
        fn step(&self, store: &mut S) -> bool {
            self.e1.step(store) >= self.e2.step(store)
        }
    }

    #[derive(Debug)]
    struct If<E1, E2, E3> {
    e1: E1,
    e2: E2,
    e3: E3,
}
    impl<S: Store, T, E1: Step<S, bool>, E2: Step<S, T>, E3: Step<S, T>> Step<S, T> for If<E1, E2, E3> {
        fn step(&self, store: &mut S) -> T {
            if self.e1.step(store) {
                self.e2.step(store)
            } else {
                self.e3.step(store)
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    struct Skip {}
    // implicitly steps to self

    #[derive(Debug)]
    struct Assign<E1, T>
    where
        T: IsValue,
{
    l: TypedLocation<T>,
    e1: E1,
}
    impl<S: Store, T, E1: Step<S, T>> Step<S, Skip> for Assign<E1, T>
    where
        T: Step<S, Value>,
        T: IsValue,
    {
        fn step(&self, store: &mut S) -> Skip {
            let v: T = self.e1.step(store);
            let v: Value = v.step(store);
            store.put(self.l.loc.clone(), v);
            Skip {}
        }
    }

    #[derive(Debug)]
    struct Deref<T: IsValue> {
    l: TypedLocation<T>,
}
    impl<S: Store, T> Step<S, T> for Deref<T>
    where
        Value: Step<S, T>,
        T: IsValue,
    {
        fn step(&self, store: &mut S) -> T {
            match store.get(&self.l.loc) {
                None => panic!("not in store! oh no!"),
                Some(v) => v.clone().step(store),
            }
        }
    }

    #[derive(Debug)]
    struct Seq<E1, E2> {
    e1: E1,
    e2: E2,
}
    impl<S: Store, T, E1: Step<S, Skip>, E2: Step<S, T>> Step<S, T> for Seq<E1, E2> {
        fn step(&self, store: &mut S) -> T {
            self.e1.step(store);
            self.e2.step(store)
        }
    }

    #[derive(Debug)]
    struct While<E1, E2> {
    e1: E1,
    e2: E2,
}
    impl<S: Store, E1: Step<S, bool>, E2: Step<S, Skip>> Step<S, Skip> for While<E1, E2> {
        fn step(&self, store: &mut S) -> Skip {
            while self.e1.step(store) {
                self.e2.step(store);
            }
            Skip {}
        }
    }

    fn new_loc<T: IsValue>(id: i32) -> TypedLocation<T> {
        TypedLocation {
            loc: Location { id },
            phantom: Default::default(),
        }
    }

    pub(crate) fn test(total: i64) {

        let l1: TypedLocation<i64> = new_loc(1);
        let l2: TypedLocation<i64> = new_loc(2);

        let main: i8 = L1!(
            (l2 := 0);
            while !l1 >= 1 do
                (l2 := !l2 + !l1);
                (l1 := !l1 + -1)
        );

        // ok lets make a precedence table
        // i want:
        // (l1 := 1); 2, NOT l1 := (1; 2)
        // but l1 := (1;2) should be respected
        // l1 := (1 + 2) NOT (l1 := 1) + 2
        // (a + b); c NOT a + (b; c)
        // while _ do (a; b) NOT (while _ do a); b
        // of expressions that end in expressions:
        // while and if_then_else are the most greedy
        // then ;
        // then := (although if it _should_ bind left is debatable)
        // and finally +

        // but how do you implement that?
        // answer: change the root midway through parsing (sounds hard)

        //   1 + 2  := 3  ;  5 + 2
        // ((1 + 2) := 3) ; (5 + 2)
        // also
        // 1 + 2 + 3
        // (1 + 2) + 3
        // just makes more sense to me, but that's a stretch

        // ok lets say: precidence
        // while, if_then_else: 5
        // ; : 4
        // := : 3
        // + : 2
        // int, bool : 1
        // (): 0

        // then we shuffle the expression tree around in post
        // eg: l := 1; 2 -> l := (1;2)[3] -> (l := (1;2)[3])[2]
        // then the l is shuffled down into the semi-colon like
        // ((l := 1)[2];(2)[1])[3]
        // until all expressions are in decreasing order, ignoring parens

        // counter example:  l := (1; 2) -> l := (1;2)[0] -> (l := (1;2)[0])[3]

        // ok this is dumb just use a global or something fuck


        let mut store = LinearStore {
            store: vec![Int(0), Int(total), Int(0)],
        };

        println!("{:?}, {:?}", main.step(&mut store), store);
    }
}
