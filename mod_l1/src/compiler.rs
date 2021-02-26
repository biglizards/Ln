pub mod l1_compiler {
    use crate::L1;
    use std::cell::UnsafeCell;
    use std::fmt;
    use std::fmt::{Display, Formatter};

    // the first major deviation from the reference model is we no longer explicitly have the store
    // instead, locations are pointers, and the store is the system's memory.
    // UnsafeCell is used because it allows the compiler to fully optimise the code
    // I'm not sure why Cell is so slow; this is basically how Cell is implemented anyway.
    #[derive(Debug)]
    pub(crate) struct Location {
        data: UnsafeCell<i64>,
    }
    impl Location {
        pub(crate) fn get(&self) -> i64 {
            // SAFETY: no-one else is accessing this location, because Location is not sync
            unsafe { *self.data.get() }
        }
        pub(crate) fn set(&self, v: i64) {
            // SAFETY: no-one else is accessing this location, because Location is not sync
            // SAFETY: you can't take a reference to the value, so nothing is invalidated.
            unsafe {
                *self.data.get() = v;
            }
        }
        pub(crate) fn from(v: i64) -> Location {
            Location {
                data: UnsafeCell::from(v),
            }
        }
    }
    impl Display for Location {
        fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.get())
        }
    }

    // we say a type steps to T if it has a function that returns a T
    // eg, Add {e1: 1, e2: 1} steps to i64
    // note that we use big step semantics (rather than small step) in many places for performance
    pub(crate) trait Step<T> {
        fn step(&self) -> T;
    }

    // small things can step to themselves -- this is needed to have integers inline,
    // since add wants two things that step to integers
    // we could use a wrapper type, but that isn't needed for L1 at least
    impl<T: Copy> Step<T> for T {
        fn step(&self) -> T {
            *self
        }
    }

    // unlike the specification, we define Add and GE independently of OP
    // it makes it easier to represent programs as types
    // you can see the semantics are equivalent (in both cases, first e1 is evaluated, then e2)
    #[derive(Debug)]
    pub(crate) struct Add<E1, E2> {
        pub(crate) e1: E1,
        pub(crate) e2: E2,
    }
    // because of the type bounds, only well typed Adds (ones in which both expressions are ints)
    // can step to int. Because of this, if an Add is not typed, any expressions which use it
    // are also not typed. That is, if an expression is Step, then it is well typed in L1.
    impl<E1: Step<i64>, E2: Step<i64>> Step<i64> for Add<E1, E2> {
        fn step(&self) -> i64 {
            self.e1.step() + self.e2.step()
        }
    }

    #[derive(Debug)]
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
    #[derive(Debug)]
    pub(crate) struct Deref<'a> {
        pub(crate) l: &'a Location,
    }
    impl<'a> Step<i64> for Deref<'a> {
        fn step(&self) -> i64 {
            self.l.get()
        }
    }

    // another argument for variety: all locations which exist are in the domain of the store
    // because we defined the store as the set of all defined locations.
    #[derive(Debug)]
    pub(crate) struct Assign<'a, E1> {
        pub(crate) l: &'a Location,
        pub(crate) e1: E1,
    }
    impl<'a, E1: Step<i64>> Step<Skip> for Assign<'a, E1> {
        fn step(&self) -> Skip {
            let v = self.e1.step();
            self.l.set(v);
            Skip {}
        }
    }

    // we fully evaluate the left expression, which we know results in a skip
    //   (because of the type binding `E1: Step<Skip>`)
    // then evaluate the right expression
    #[derive(Debug)]
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
    #[derive(Debug)]
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

    // Skip implicitly steps to self because it is Copy
    // but can make no other progress
    #[derive(Clone, Copy, Debug)]
    pub(crate) struct Skip {}

    // While just does an actual loop instead of the recursive definition,
    // but the two are clearly the same
    #[derive(Debug)]
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

    pub fn test_sum(total: i64) -> i64 {
        let l1 = Location::from(total);
        let l2 = Location::from(0);

        let main = L1!(
            l2 := 0;
            while !l1 >= 1 do
                l2 := !l2 + !l1;
                l1 := !l1 + -1
        );

        main.step();
        l2.get()
    }
}
