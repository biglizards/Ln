# L_n: A compiler for L1 (sort of)

A Rust implementation of the L1 language, as specified in 
[this course](https://www.cl.cam.ac.uk/teaching/2021/Semantics/). This commit still has a lot of 
old commented out code and notes in, which I guess you might find interesting.
 
Currently this implements a superset of L1, as the store may contain any value. This will be fixed when the repo
is split into different folders for each language.

Contains both an interpreter and a compiler, so long as you accept that Rust is a valid compiler backend.
(The interpreter is currently out of sync with the compiler, but again that'll be fixed soon).
See below for an example of how you might run an L1 program to compute the sum of all numbers
between 1 and 1 billion.

```rust
// define the store -- a mapping from locations to value references
// stricly speaking it should only allow intrefs in L1.
// L1 has no way to create new locations,
// so it can only run with what we give it here.
let mut store = LinearStore {
    store: vec![Int(10000000000), Int(0)],
};

// Manually define locations. The argument is the index in the store.
// these variables are accessed from within the L1 code, so names matter.
let l1 = new_loc(0);
let l2 = new_loc(1);

// Converts the L1 code into the rust representation, ready for execution.  
let program = L1!(
    l2 := 0;
    while !l1 >= 1 do
        l2 := !l2 + !l1;
        l1 := !l1 + -1
);

// run the program to completion, and print the "results"
program.step(&mut store);
println!("the final state of the store is: {:?}",  store);
``` 

Due to the way programs are represented as types*, rustc is able to heavily optimise L1 programs 
(although not quite as much as if they were written in Rust directly): it takes `0.39` seconds to
run the above program on my machine, implying each iteration of the loop takes just over one 
clock cycle.

\*The type of the above program is
```
Seq<Assign<{integer}, i64>, While<GE<ct::Deref<i64>, {integer}>, Seq<Assign<ct::Add<ct::Deref<i64>, ct::Deref<i64>>, i64>, Assign<ct::Add<ct::Deref<i64>, {integer}>, i64>>>>
```
In addition to speed, this also uses Rust's type system to verify the correctness of all programs, as `step` is only
defined for well-typed expressions.