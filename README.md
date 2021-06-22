# Ln: A compiler for L1 and L2

A Rust implementation of the L* family of languages, as specified in 
[this course](https://www.cl.cam.ac.uk/teaching/2021/Semantics/).
 
Currently this implements both L1 and L2 fully, with support for L3 in progress.

Contains both an interpreter and a compiler, so long as you accept that Rust is a valid compiler backend 
(otherwise, you would, perhaps more accurately, call this a transpiler).
See below for an example of how you might run an L1 program to compute the sum of all numbers
between 1 and 1 billion (storing the result in `l2`).

```rust
// Manually define locations (L1 requires all locations are initialised before execution)
// These variables are accessed from within the L1 code, so names matter.
let l1 = Location::from(10000000000);
let l2 = Location::from(0);

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

Due to the way programs are represented as types*, rustc and LLVM are able to heavily optimise L1 programs: 
the program above is compiled down to 14 lines of x86 ASM, with no backwards branches.

In addition to speed, this also uses Rust's type system to verify the correctness of all programs, as `step` is only
defined for well-typed expressions.

\*The type of the above program is
```
Seq<Assign<'_, {integer}>, While<GE<Deref<'_>, {integer}>, Seq<Assign<'_, Add<Deref<'_>, Deref<'_>>>, Assign<'_, Add<Deref<'_>, {integer}>>>>>
```


## L2

L2 primarily adds functions, variables, and let bindings. The sum example above could also have been written using
recursion:

```rust
let program = L2!(
    let val rec sum: int->int = (
        fn x: int => if 0 >= x then 0 else x + sum (x + -1)
    ) in (
        l2 := sum !l1
    ) end
)
```
