# mod_l1

Annoyingly I have to define `Expression` three times in this crate (and probably all subsequent crates), but they have
different requirements in each case so I don't think it can be cut down sensibly.

There's also currently no way to change the code the interpreter interprets at runtime, because I don't have a runtime
parser and can't be bothered to make one.

Also, for whatever reason, you need to put the code you want to compile in `compiler.rs`. If you make the structs public
and try to call it from main it doesn't optimise it as much. In the future I'll get the macros to just copy that file
into wherever you're using it.