# L_n: A compiler for L1 (sort of)

A Rust implementation of the L1 language, as specified in 
[this course](https://www.cl.cam.ac.uk/teaching/2021/Semantics/). This commit still has a lot of 
old commented out code and notes in, which I guess you might find interesting.
 
Currently this implements a superset of L1, as the store may contain any value. This will be fixed when the repo
is split into different folders for each language.

Contains both an interpreter and a compiler, so long as you accept that Rust is a valid compiler backend.
(The interpreter is currently out of sync with the compiler, but again that'll be fixed soon).
See below for an example of how you might run an L1 program to compute the sum of all numbers
between 1 and 1 billion (storing the result in `l2`).

```rust
// Manually define locations (L1 requires all locations are initialised before execution)
// These variables are accessed from within the L1 code, so names matter.
let l1 = new_loc(10000000000);
let l2 = new_loc(1);

// Converts the L1 code into the rust representation, ready for execution.  
let program = L1!(
    l2 := 0;
    while !l1 >= 1 do
        l2 := !l2 + !l1;
        l1 := !l1 + -1
);

// run the program to completion, and print the "results"
program.step();
println!("the final state is: {}, {}", l1, l2);
``` 

Due to the way programs are represented as types*, rustc is able to heavily optimise L1 programs 
(although not quite as much as if they were written in Rust directly): it takes `0.39` seconds to
run the above program on my machine, implying each iteration of the loop takes just over one 
clock cycle.

\*The type of the above program is
```
Seq<Assign<'_, {integer}, i64>, While<GE<Deref<'_, i64>, {integer}>, Seq<Assign<'_, Add<Deref<'_, i64>, Deref<'_, i64>>, i64>, Assign<'_, Add<Deref<'_, i64>, {integer}>, i64>>>>
```
In addition to speed, this also uses Rust's type system to verify the correctness of all programs, as `step` is only
defined for well-typed expressions.