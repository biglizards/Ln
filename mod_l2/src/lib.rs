// #![feature(type_ascription)]

mod compiler;
mod interpreter;
extern crate self as mod_l2;

extern crate l2_parser;

pub use compiler::l2_compiler;
pub use interpreter::l2_interpreter;

pub use l2_parser::L2_interpreter;
pub use l2_parser::L2;

#[cfg(test)]
mod tests {
    use crate::interpreter::l2_interpreter::{Expression, Variable};
    use crate::l2_compiler::Step;
    use crate::l2_interpreter::Expression::VALUE;
    use crate::l2_interpreter::{step, LinearStore, Location, Store};
    use l2_parser::L2_interpreter;
    use std::time::Instant;

    /// fully evaluate a program in a given store
    /// returns the store if the program evaluates fine (ie it evaluates to a value)
    /// returns Err if it gets stuck (ie the program is not well typed)
    fn evaluate<S: Store>(mut main: Expression, mut store: S) -> Result<S, ()> {
        while let Some(e) = step(main, &mut store) {
            main = e;
            if let Expression::SKIP = main {
                return Ok(store);
            };
            if let VALUE(_) = main {
                return Ok(store);
            }
        }
        return Err(());
    }

    #[test]
    fn interpreter_sum_test() {
        let l1 = Location { id: 0 };
        let l2 = Location { id: 1 };

        let store = LinearStore {
            store: vec![10000, 0],
            var_store: vec![],
        };

        let main = L2_interpreter!(
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
    fn interpreter_basic_function() {
        let l1 = Location { id: 0 };
        let x = Variable { id: 0 };

        let store = LinearStore {
            store: vec![10000, 0],
            var_store: vec![0],
        };

        let main = L2_interpreter!(
            l1 := ((fn x: int => x + 3 ) 4)
        );

        // implicitly asserts that the program did not get stuck
        let sum = *evaluate(main, store).unwrap().get(&l1).unwrap();
        assert_eq!(sum, 7);
    }

    #[test]
    fn interpreter_higher_order_function_1() {
        // function which takes a function as an argument
        let l1 = Location { id: 0 };
        let x = Variable { id: 0 };
        let y = Variable { id: 1 };

        let store = LinearStore {
            store: vec![10000, 0],
            var_store: vec![],
        };

        let main = L2_interpreter!(
            l1 := ((fn x: int->int => x 3) (fn y: int => y + 4))
        );

        // implicitly asserts that the program did not get stuck
        let sum = *evaluate(main, store).unwrap().get(&l1).unwrap();
        assert_eq!(sum, 7);
    }

    #[test]
    fn interpreter_higher_order_function_2() {
        // function which returns a function
        let l1 = Location { id: 0 };
        let x = Variable { id: 0 };
        let y = Variable { id: 1 };

        let store = LinearStore {
            store: vec![10000, 0],
            var_store: vec![],
        };

        let main = L2_interpreter!(
            l1 := (((fn x: int => fn y: int => x + y) 3) 4)
        );

        // implicitly asserts that the program did not get stuck
        let sum = *evaluate(main, store).unwrap().get(&l1).unwrap();
        assert_eq!(sum, 7);
    }

    #[test]
    fn interpreter_higher_order_function_3() {
        // function which takes and returns a function
        let l1 = Location { id: 0 };
        let x = Variable { id: 0 };
        let y = Variable { id: 1 };

        let store = LinearStore {
            store: vec![10000, 0],
            var_store: vec![],
        };

        let main = L2_interpreter!(
            l1 := (((fn x: int->int => fn y: int => (x y) + 2) (fn x: int => x + 4)) 1)
        );

        // implicitly asserts that the program did not get stuck
        let sum = *evaluate(main, store).unwrap().get(&l1).unwrap();
        assert_eq!(sum, 7);
    }

    #[test]
    fn interpreter_silly_test() {
        let l1 = Location { id: 0 };
        let store = LinearStore {
            store: vec![123],
            var_store: vec![],
        };

        let main = L2_interpreter!(
            l1 := false
        );

        assert!(evaluate(main, store).is_err());
    }

    #[test]
    fn compiler_sum_test() {
        let sum = crate::l2_compiler::test_sum(10000);
        assert_eq!(sum, 50005000);
    }

    #[test]
    fn compiler_fn_test() {
        let sum = crate::l2_compiler::test_add(3);
        assert_eq!(sum, 5);
    }

    #[test]
    fn compiler_stack_test() {
        let sum = crate::l2_compiler::test_stack(3);
        assert_eq!(sum, 6);
    }

    #[test]
    fn compiler_test_higher_order_1() {
        let sum = crate::l2_compiler::test_higher_order_1(3);
        assert_eq!(sum, 5);
    }

    #[test]
    fn compiler_test_higher_order_2() {
        let sum = crate::l2_compiler::test_higher_order_2(3);
        assert_eq!(sum, 5);
    }

    #[test]
    fn compiler_test_higher_order_3() {
        let sum = crate::l2_compiler::test_higher_order_3(3);
        assert_eq!(sum, 9);
    }

    #[test]
    fn compiler_test_multiple_references() {
        let sum = crate::l2_compiler::test_multiple_references(3);
        assert_eq!(sum, 5);
    }

    #[test]
    fn compiler_test_multiple_use() {
        let sum = crate::l2_compiler::test_multiple_use(3);
        assert_eq!(sum, 8);
    }

    #[test]
    fn compiler_let_binding_test_1() {
        let sum = crate::l2_compiler::test_let_bindings_2(3);
        assert_eq!(sum, 7);
    }

    #[test]
    fn compiler_let_binding_test_2() {
        let sum = crate::l2_compiler::test_let_bindings_3(3);
        assert_eq!(sum, 13);
    }

    // #[test]
    // fn compiler_let_rec_optimisation_test() {
    // }

    // #[test]
    // fn test_4() {
    //     let mut f: UnsafeCell<Box<dyn Fn(i64)->i64>> = UnsafeCell::new(Box::from(|x: i64| {0}));
    //
    //     print!("what\n");
    //
    //     // let y = {&mut g as *mut Fn()};
    //     let tmp_func = |x| {
    //         if x == 0 {
    //             1
    //         } else {(unsafe {*f.get()})(x-1) }
    //     };
    //
    //     unsafe {*(f.get()) = Box::from(tmp_func);}
    //
    //     println!("{}", (unsafe {**f.get()})(1));
    // }

    #[test]
    fn compiler_basic_recursive_function() {
        let sum = crate::l2_compiler::test_basic_rec_fn(5);
        assert_eq!(sum, 15)
    }

    #[test]
    fn compiler_recursive_function() {
        let sum = crate::l2_compiler::test_rec_fn(5);
        assert_eq!(sum, 20)
    }

    #[test]
    fn compiler_recursive_function2() {
        let sum = crate::l2_compiler::test_rec_fn_2(5);
        assert_eq!(sum, 15)
    }

    /// make sure it compiles the loop to an O(1) function.
    #[test]
    fn compiler_speed_test() {
        // yes, i know, testing for speed is generally a bad idea
        // but in this case it compiles down to like 10 instructions so we should be good
        // i've put the time 2 orders of magnitude slower than on my machine
        // really what you want to do is check the asm by hand
        let now = Instant::now();

        let l1 = super::l2_compiler::Location::from(4200000000);
        let l2 = super::l2_compiler::Location::from(0);

        let main = crate::L2!(
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
}
