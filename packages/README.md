# This is where Musi's bootstrapped compiler will be

It will replace OCaml's lib/musi_*/ implementations 1-by-1.

Might wanna figure out how to make an executable or musi_driver that runs the lexer and hopefully manages to somehow pass specifics?
Likely we'd have to implement a sort of libstd/assert similar to @std/assert of JSR to be able to test stuff the Deno way!
