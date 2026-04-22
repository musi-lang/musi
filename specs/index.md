# Specs

Status: proposed

## Host-Language Comparison Baseline

This compiler/runtime is implemented in Rust 2024 edition with workspace `rust-version = "1.87.0"`.

Specs may compare Musi semantics against Rust when the comparison clarifies an implementation boundary. Rust is a host and reference point, not source syntax authority for Musi.

Comparison rules:

- cite Rust for memory safety, aliasing, unsafe, ABI, runtime implementation tradeoffs, and delimiter comparisons where Musi intentionally diverges or matches surface shape
- do not import Rust source terms such as traits, impl blocks, lifetimes, borrow sigils, `?`, or `Result<T, E>` syntax into Musi surface design
- expose user-visible consequences in Musi terms: `given`, `answer`, `handle`, `?T`, `E!T`, `:=`, `=`, `/=`, `{}` structure, `()` computing

## Canonical Map

Canonical spec map:

- `specs/language/`: first-class design, source syntax, types, contextual values, effects, attributes, and module boundaries
- `specs/runtime/`: managed memory and runtime interop rules
- `specs/interop/`: C boundary mapping rules
- `specs/seam/`: lowered SEAM bytecode, domains, module format, and lowering contract
