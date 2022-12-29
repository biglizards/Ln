# mod_l2

Make sure to read the mod_l1 readme as well.

Functions are hard to compile. On one hand, simple functions like `(fn x: int => x+1) !l1` should compile down to
no more than a few lines of asm. More complex ways you can use functions include:

1. taking multiple arguments: `(fn x: int => fn y: int => x+y) !l1 !l2` is `!l1 + !l2`
    - partial application: `(fn x: int => fn y: int => x+y) !l1`
        - not actually that bad without eg higher order functions
2. higher order functions
    1. returning functions: `(fn x:int => (fn y: int => y+1)) !l1`
    2. taking functions as arguments: `(fn x: int->int => x 1) (fn x: int => x+1)` is `2`
    
if a function is partially applied then returned, it must be at least partially generated dynamically (argument is not
known at compile time) and cannot be stack allocated (since it is returned and thus invalidated).

Recursion and variables allow an arbitrarily large number of function objects to be valid at once.

~~It feels impossible to avoid the conclusion that, for some functions, large reference counted objects will need
to be created.~~

NOTE FROM FUTURE ME: I wrote the above readme maybe 2 months before I took the compilers course, and they had a neat solution: take all the lambdas, and make them into top-level functions. Then you represent lambdas as a function pointer and a tuple of all unbound variables. If I give this another go at some point (and I'm unlikely to, but not entirely taking it off the table), this would likely yeild both performance improvements and increased expressiveness in terms of operational semantics, at the cost of a more complex macro side.
