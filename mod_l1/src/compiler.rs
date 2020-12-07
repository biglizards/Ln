pub mod l1_compiler {
    use crate::Value::{Bool, Int};
    use crate::L1;
    use std::rc::Rc;
    use std::cell::{UnsafeCell};
    use std::borrow::Borrow;
    use std::fmt::{Formatter, Display};
    use std::fmt;

    trait IsValue {}
    impl IsValue for i64 {}
    impl IsValue for bool {}

    #[derive(Debug)]
    struct TypedLocation<T: IsValue + Copy> {
    data: UnsafeCell<T>
}
    impl<T: IsValue + Copy> TypedLocation<T> {
        fn get(&self) -> T {
            unsafe {
                *self.data.get()
            }
        }
        fn set(&self, v: T) {
            unsafe {
                let data: *mut T = self.data.get();
                *data = v;
            }
        }
    }
    impl<T: IsValue + Copy + Display> Display for TypedLocation<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.get())
        }
    }



    trait Step<T> {
        fn step(&self) -> T;
    }

    impl<T: Copy> Step<T> for T {
        fn step(&self) -> T {
            *self
        }
    }

    #[derive(Debug)]
    struct Add<E1, E2> {
        e1: E1,
        e2: E2,
    }
    impl<E1: Step<i64>, E2: Step<i64>> Step<i64> for Add<E1, E2> {
        fn step(&self) -> i64 {
            self.e1.step() + self.e2.step()
        }
    }
    // impl<T, E1: Step<i64>, E2: Step<i64>> Step<T> for Add<E1, E2>
    // where
    //     i64: Step<T>,
    // {
    //     fn step(&self) -> T {
    //         (self.e1.step(store) + self.e2.step(store)).step(store)
    //     }
    // }

    #[derive(Debug)]
    struct GE<E1, E2> {
        e1: E1,
        e2: E2,
    }
    impl<E1: Step<i64>, E2: Step<i64>> Step<bool> for GE<E1, E2> {
        fn step(&self) -> bool {
            self.e1.step() >= self.e2.step()
        }
    }

    #[derive(Debug)]
    struct If<E1, E2, E3> {
        e1: E1,
        e2: E2,
        e3: E3,
    }
    impl<T, E1: Step<bool>, E2: Step<T>, E3: Step<T>> Step<T> for If<E1, E2, E3> {
        fn step(&self) -> T {
            if self.e1.step() {
                self.e2.step()
            } else {
                self.e3.step()
            }
        }
    }

    #[derive(Clone, Copy, Debug)]
    struct Skip {}
    // implicitly steps to self

    #[derive(Debug)]
    struct Assign<'a, E1, T>
        where
            T: IsValue + Copy,
    {
        l: &'a TypedLocation<T>,
        e1: E1,
    }

    impl<'a, T, E1: Step<T>> Step<Skip> for Assign<'a, E1, T>
        where
            T: IsValue + Copy,
    {
        fn step(&self) -> Skip {
            let v: T = self.e1.step();
            self.l.set(v);
            Skip {}
        }
    }

    #[derive(Debug)]
    struct Deref<'a, T: IsValue + Copy> {
        l: &'a TypedLocation<T>,
    }
    impl<'a, T> Step<T> for Deref<'a, T>
        where
            T: IsValue + Copy
    {
        fn step(&self) -> T {
            self.l.get()
        }
    }

    #[derive(Debug)]
    struct Seq<E1, E2> {
        e1: E1,
        e2: E2,
    }
    impl<T, E1: Step<Skip>, E2: Step<T>> Step<T> for Seq<E1, E2> {
        fn step(&self) -> T {
            self.e1.step();
            self.e2.step()
        }
    }

    #[derive(Debug)]
    struct While<E1, E2> {
        e1: E1,
        e2: E2,
    }
    impl<E1: Step<bool>, E2: Step<Skip>> Step<Skip> for While<E1, E2> {
        fn step(&self) -> Skip {
            while self.e1.step() {
                self.e2.step();
            }
            Skip {}
        }
    }

    fn new_loc<T: IsValue + Copy>(data: T) -> TypedLocation<T> {
        TypedLocation {
            data: UnsafeCell::from(data)
        }
    }

    pub fn test(total: i64) {
        let l1: TypedLocation<i64> = new_loc(total);
        let l2: TypedLocation<i64> = new_loc(0);

        let main = L1!(
            l2 := 0;
            while !l1 >= 1 do
                l2 := !l2 + !l1;
                l1 := !l1 + -1
        );

        println!("{:?}, {}, {}", main.step(), l1, l2);
    }
}
