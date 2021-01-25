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

It feels impossible to avoid the conclusion that, for some functions, large reference counted objects will need
to be created.