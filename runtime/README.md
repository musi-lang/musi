# The Rules of Crust

1. Every function is `unsafe`.
1. No references, only pointers.
1. No cargo, build with rustc directly.
1. No std, but `libc` is allowed.
1. Only Edition 2021 and version `1.56.0` minimum.
1. All user structs and enums `#[derive(Clone, Copy)]`.
1. Everything is `pub` by default.

*The list of rules may change. The goal is to make programming in Rust fun.*

## Uses

Currently used in the [B Compiler Project](https://github.com/tsoding/b).
Will be used in the [Musi Compiler Project](https://github.com/musi-lang-new/musi).
