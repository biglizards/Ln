mod compiler;
mod interpreter;
extern crate self as mod_l1;

extern crate l1_parser;


pub use compiler::l1_compiler;
pub use interpreter::l1_interpreter;
pub use l1_parser::L1;

#[cfg(test)]
mod tests {
    use std::time::Instant;
    use l1_parser::L1_interpreter;
    use crate::l1_interpreter::{Location, LinearStore, Store, step};
    use crate::interpreter::l1_interpreter::Expression;
    use crate::l1_interpreter::Expression::VALUE;
    use crate::l1_compiler::Step;

    /// fully evaluate a program in a given store
    /// returns the store if the program evaluates fine (ie it evaluates to a value)
    /// returns Err if it gets stuck (ie the program is not well typed)
    pub(crate) fn evaluate<S: Store>(mut main: Expression, mut store: S) -> Result<S, ()> {
        while let Some(e) = step(main, &mut store)  {
            main = e;
            if let Expression::SKIP = main {
                return Ok(store);
            };
            if let VALUE(_) = main {
                return Ok(store);
            }
        }
        return Err(())
    }


    #[test]
    fn interpreter_sum_test() {
        let l1 = Location { id: 0 };
        let l2 = Location { id: 1 };

        let store = LinearStore { store: vec![10000, 0] };

        let main = L1_interpreter!(
            l2 := 0;
            while !l1 >= 1 do
                l2 := !l2 + !l1;
                l1 := !l1 + -1
        );

        // implicitly asserts that the program did not get stuck
        let sum = *evaluate(main, store).unwrap().get(&l2).unwrap();
        assert_eq!(sum, 50005000);
    }

    #[test]
    fn compiler_sum_test() {
        let sum = crate::l1_compiler::test_sum(10000);
        assert_eq!(sum, 50005000);
    }

    /// make sure it compiles the loop to an O(1) function.
    #[test]
    fn compiler_speed_test() {
        // yes, i know, testing for speed is generally a bad idea
        // but in this case it compiles down to like 10 instructions so we should be good
        // i've put the time 2 orders of magnitude slower than on my machine
        let now = Instant::now();

        let l1 = super::l1_compiler::Location::from(4200000000);
        let l2 = super::l1_compiler::Location::from(0);

        let main = crate::L1!(
            l2 := 0;
            while !l1 >= 1 do
                l2 := !l2 + !l1;
                l1 := !l1 + -1
        );

        main.step();

        let time = (now.elapsed().as_nanos() as f64) / 1_000_000_000_f64;
        assert_eq!(l2.get(), 8820000002100000000);
        assert!(time < 0.00001)
    }

    #[test]
    fn interpreter_silly_test() {
        let l1 = Location { id: 0 };
        let store = LinearStore { store: vec![123] };

        let main = L1_interpreter!(
            l1 := false
        );

        assert!(evaluate(main, store).is_err());
    }

}
