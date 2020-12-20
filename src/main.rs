extern crate mod_l1;
// extern crate mod_l2;

use mod_l1::l1_compiler;

use std::env;
use std::time::Instant;

fn main() {
    let args: Vec<String> = env::args().collect();
    let s: String = "1000000000".into();

    let now = Instant::now();
    l1_compiler::test_sum(
        (&args.get(1).unwrap_or(&s))
            .parse()
            .expect("that's not an int, fool"),
    );
    let time = (now.elapsed().as_nanos() as f64) / 1_000_000_000_f64;
    println!("{:?}", time);
}
