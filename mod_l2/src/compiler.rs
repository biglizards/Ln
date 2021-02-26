pub mod l2_compiler {
    use crate::L2;
    use std::cell::UnsafeCell;
    use std::fmt;
    use std::fmt::{Debug, Display, Formatter};
    use std::marker::PhantomData;
    use std::rc::Rc;

    // the first major deviation from the reference model is we no longer explicitly have the store
    // instead, locations are pointers, and the store is the system's memory.
    // UnsafeCell is used because it allows the compiler to fully optimise the code
    // I'm not sure why Cell is so slow; this is basically how Cell is implemented anyway.
    #[derive(Debug, Clone, Copy)]
    pub(crate) struct TypedLocation<T> {
        ptr: *mut T,
    }
    impl<T> TypedLocation<T> {
        pub(crate) fn from(v: T) -> TypedLocation<T> {
            // this is not a very good solution because it leaks memory but w/e
            TypedLocation {
                ptr: Box::leak(Box::from(UnsafeCell::new(v))).get(),
            }
        }
    }
    impl<T: Clone + Display> Display for TypedLocation<T> {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.get())
        }
    }
    // i think we should avoid cloning it if possible, but returning references is obviously unsafe
    impl<T: Clone> TypedLocation<T> {
        pub(crate) fn get(&self) -> T {
            // SAFETY: no-one else is accessing this location, because Location is not sync
            unsafe { (*self.ptr).clone() }
        }
        pub(crate) fn set(&self, v: T) {
            // SAFETY: no-one else is accessing this location, because Location is not sync
            // SAFETY: you can't take a reference to the value, so nothing is invalidated.
            unsafe {
                *self.ptr = v;
            }
        }
    }

    pub(crate) type Location = TypedLocation<i64>;

    // we say a type steps to T if it has a function that returns a T
    // eg, Add {e1: 1, e2: 1} steps to i64
    // note that we use big step semantics (rather than small step) in many places for performance
    pub(crate) trait Step<T> {
        fn step(&self) -> T;
    }

    // small things can step to themselves -- this is needed to have integers inline,
    // since add wants two things that step to integers
    // we could use a wrapper type, but that isn't needed for L1 at least
    // impl<T: Copy> Step<T> for T {
    //     fn step(&self) -> T {
    //         *self
    //     }
    // }

    // ok so, the blanket Copy impl _seems_ reasonable, but it is in fact completely unreasonable
    // i'm just doing a specific one for i64 unless there's a really good reason not to
    impl Step<i64> for i64 {
        fn step(&self) -> i64 {
            *self
        }
    }
    impl Step<()> for () {
        fn step(&self) -> () {
            *self
        }
    }
    impl Step<bool> for bool {
        fn step(&self) -> bool {
            *self
        }
    }

    // unlike the specification, we define Add and GE independently of OP
    // it makes it easier to represent programs as types
    // you can see the semantics are equivalent (in both cases, first e1 is evaluated, then e2)
    #[derive(Debug, Clone)]
    pub(crate) struct Add<E1, E2> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
    }
    // because of the type bounds, only well typed Adds (ones in which both expressions step to int)
    // can step to int. Because of this, if an Add is not typed, any expressions which use it
    // are also not typed. That is, if an expression is Step, then it is well typed in L1.
    impl<E1: Step<i64>, E2: Step<i64>> Step<i64> for Add<E1, E2> {
        fn step(&self) -> i64 {
            self.e1.step() + self.e2.step()
        }
    }

    #[derive(Debug, Clone)]
    pub(crate) struct GE<E1, E2> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
    }
    impl<E1: Step<i64>, E2: Step<i64>> Step<bool> for GE<E1, E2> {
        fn step(&self) -> bool {
            self.e1.step() >= self.e2.step()
        }
    }

    // Since the program type checks, we have progress, and since we have progress,
    // we know deref won't get stuck, so it must be safe to deref this location.
    #[derive(Debug, Clone)]
    pub(crate) struct Deref {
        pub(crate) l: Location,
    }
    impl Step<i64> for Deref {
        fn step(&self) -> i64 {
            self.l.get()
        }
    }

    // another argument for variety: all locations which exist are in the domain of the store
    // because we defined the store as the set of all defined locations.
    #[derive(Debug, Clone)]
    pub(crate) struct Assign<E1> {
        pub(crate) l: Location,
        pub(crate) e1: E1,
    }
    impl<E1: Step<i64>> Step<Skip> for Assign<E1> {
        fn step(&self) -> Skip {
            let v = self.e1.step();
            self.l.set(v);
            Skip {}
        }
    }

    // we fully evaluate the left expression, which we know results in a skip
    //   (because of the type binding `E1: Step<Skip>`)
    // then evaluate the right expression
    #[derive(Debug, Clone)]
    pub(crate) struct Seq<E1, E2> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
    }
    impl<T, E1: Step<Skip>, E2: Step<T>> Step<T> for Seq<E1, E2> {
        fn step(&self) -> T {
            self.e1.step();
            self.e2.step()
        }
    }

    // similarly, we fully evaluate the condition into a bool,
    // then return the result of the correct sub-expression.
    // the typing rules require both subexpressions to be of the same type
    #[derive(Debug, Clone)]
    pub(crate) struct If<E1, E2, E3> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
        pub(crate) e3: E3,
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

    // Skip steps to self for the same reasons i64 does
    // but can make no other progress
    #[derive(Clone, Copy, Debug)]
    pub(crate) struct Skip {}
    impl Step<Skip> for Skip {
        fn step(&self) -> Skip {
            *self
        }
    }

    // While just does an actual loop instead of the recursive definition,
    // but the two are clearly the same
    #[derive(Debug, Clone)]
    pub(crate) struct While<E1, E2> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
    }
    impl<E1: Step<bool>, E2: Step<Skip>> Step<Skip> for While<E1, E2> {
        fn step(&self) -> Skip {
            while self.e1.step() {
                self.e2.step();
            }
            Skip {}
        }
    }

    // ------------------------------
    //          newer stuff
    // ------------------------------

    #[derive(Debug, Clone)]
    pub(crate) struct Apply<E1, E2, In> {
        e1: E1,
        e2: E2,
        phantom: PhantomData<In>,
    }
    impl<T, E1, E2, In> Step<T> for Apply<E1, E2, In>
    where
        E1: Step<Rc<dyn Fn(In) -> T>>,
        E2: Step<In>,
    {
        fn step(&self) -> T {
            return (*self.e1.step())(self.e2.step());
        }
    }

    impl<In, T> Step<Rc<dyn Fn(In) -> T>> for Rc<dyn Fn(In) -> T> {
        fn step(&self) -> Rc<dyn Fn(In) -> T> {
            self.clone()
        }
    }

    // optimisation: using direct function pointers instead of RC
    // obviously this can only be used when we can guarantee the pointer doesn't go out
    // of the scope it was defined in. this is usually the case, but hard to check in general,
    // so this optimisation is only used in the few cases where i can prove soundness.
    impl<T, E2, In> Step<T> for Apply<fn(In) -> T, E2, In>
    where
        E2: Step<In>,
    {
        fn step(&self) -> T {
            return (self.e1)(self.e2.step());
        }
    }

    pub fn test_sum(total: i64) -> i64 {
        let l1 = Location::from(total);
        let l2 = Location::from(0);

        let main = L2!(
            l2 := 0;
            while !l1 >= 1 do
                l2 := !l2 + !l1;
                l1 := !l1 + -1
        );

        main.step();
        return l2.get();
    }

    pub fn test_add(arg: i64) -> i64 {
        let main = L2!(
            ((fn x: int => x + 2 ) arg)
        );

        return main.step();
    }

    pub fn test_stack(arg: i64) -> i64 {
        let main = L2! {
            ((fn x:int => ((fn x:int => x+1)2)+x) arg)
        };
        return main.step();
    }

    pub fn test_higher_order_1(arg: i64) -> i64 {
        // function which takes a function as an argument
        // here we use the "apply" function which takes "add_2" as an argument
        // but neither are actually named

        let main = L2! {
            ((fn x: int->int => x arg) (fn x:int => x + 2))
        };

        return main.step();
    }

    pub fn test_higher_order_2(arg: i64) -> i64 {
        // function which returns a function
        // aka it takes two arguments
        let main = L2! {
            (((fn x: int => fn y: int => x + y) 2) arg)
        };
        return main.step();
    }

    pub fn test_higher_order_3(arg: i64) -> i64 {
        // function which takes and returns a function
        // aka a decorator
        let main = L2! {
            (((fn x: (int->int) => fn y: int => (x y) + 2) (fn x: int => x + 4)) arg)
        };

        return main.step();
    }

    pub fn test_multiple_references(arg: i64) -> i64 {
        // in this test i want to have a function be aliased, as f and g
        let main = L2! {
            ((fn main: (int->int)->(int->int)->int => fn f: int->int => (main f) f)
            (fn f: int -> int => fn g: int -> int => f (g arg)))
            (fn x: int => x + 1)
        };

        return main.step();
    }

    pub fn test_multiple_use(arg: i64) -> i64 {
        let main = L2! {
            (fn f: int->int => ((fn x: int => f x) arg) + ((fn x: int => f x) arg)) (fn x: int => x+1)
        };

        main.step()
    }

    pub fn test_call_by_value() {
        let l = Location::from(0);

        (L2! {
            (fn x: unit => (l:=1); x) (l:=2)
        })
        .step();

        assert_eq!(l.get(), 1)
    }

    // pub fn test_let_bindings_1(arg: i64) {
    //     // should fail
    //     let main = L2! {
    //         let var x: int = arg in (
    //             (let var y: int = 2 in (
    //                 y+x
    //             )) +
    //             (let var z: int = 3 in (
    //                 y+z+x
    //             ))
    //         )
    //     };
    // }

    pub fn test_let_bindings_2(arg: i64) -> i64 {
        let main = L2! {
            let val x: int = arg in
                let val x: int = x+1 in
                    x
                end + x
            end
        };
        main.step()
    }

    pub fn test_let_bindings_3(arg: i64) -> i64 {
        // this function does (arg*4)+1
        let main = L2! {
            let val x: int = arg in
                (fn x: int =>
                    let val x: int = x+1 in
                        x+arg
                    end + x
                ) x + (let val x: int = x in x end)
            end
        };
        main.step()
    }

    pub fn test_rec_fn(arg: i64) -> i64 {
        // takes the sum of the numbers up to arg
        // but does so without directly applying the recursive function
        // this is to make sure any optimisations don't break the assumption that
        // x is defined inside of itself, and it captures scope from outside

        let main = L2! {
            let val a: int = arg in (
                let val rec x: int->int = (
                    fn y: int => if 0 >= y then a else y + ((
                        fn b: bool => if b then x else (fn y: int => y)
                    ) true) (y + -1)
                ) in x(arg) end)
            end
        };
        main.step()
    }

    pub fn test_rec_fn_2(arg: i64) -> i64 {
        // similar to test_rec_fn, but now we have a function in the outer scope

        let main = L2! {
            let val add: int->int->int = (fn x: int => fn y: int => x+y) in
                (let val rec x: int->int = (
                    fn y: int => if 0 >= y then 0 else add y (x (add y -1))
                ) in x(arg) end)
            end
        };
        main.step()
    }

    pub fn test_basic_rec_fn(arg: i64) -> i64 {
        let main = L2! {
            let val rec sum: int->int = (
                fn x: int => if 0>=x then 0 else x + sum (x+-1)
            ) in sum arg end
        };
        main.step()
    }
}
