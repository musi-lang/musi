# Builtin registry

Musi builtins use a hidden catalog, not a public raw `Builtin` module.

- `music_builtin` owns compiler builtin names, foundation module specs, std package file paths, and intrinsic symbols.
- `musi:core`, focused `musi:*` host/VM primitive modules, and `@std/*` expose stable public APIs over that hidden layer.
- `musi:intrinsics` is registered for compiler-owned internals and is not a user import surface.
- `@musi.builtin` marks compiler builtin source names such as `Type`.
- `Option` and `Result` are ordinary library types. Source sugar `?T`, `E!T`, `??`, and `catch` lowers through those public types.
- `Range[Bound]` is the single canonical range value. It follows Swift's `Range<Bound> where Bound: Comparable` shape: endpoints are values of `Bound`, containment uses comparison evidence, and sequence behavior needs stepping evidence.
- Range endpoint inclusivity lives in the range value, so `a .. b`, `a ..< b`, `a <.. b`, and `a <..< b` share `Range[Bound]`.
- `Bits[N]` is the fixed-width bit-vector type used by logical operator instances that cannot be represented by ordinary `Int`/`Nat` arithmetic. `@std/bits` provides Musi-source constructors, conversions, predicates, and named logical helpers for common bit widths.
- `Pin[T]` is compiler builtin scoped pin capability created only by a `pin` action inside an `unsafe` block.
- Intrinsics carry a JIT lowering contract: Cranelift opcode, Cranelift trap, runtime call, VM-only, or unsupported-for-JIT.
- Filesystem, process, random, datetime, formatting primitives, cryptographic kernels, byte/string primitives, and target facts use focused host/VM primitive modules. Stdlib algorithms such as path manipulation, hex encoding, text predicates, and logging composition stay in `@std/*` Musi source.
- `@std` is a Rust/Scala-style barrel over focused exports. Public modules stay grouped by concern; raw host facts such as `sys` stay private implementation details behind typed modules such as `@std/os`.
- Native boundary access is modeled through `native`, `@link`, explicit unsafe surfaces, and ordinary import/export records.

This keeps user code on stdlib APIs while giving future `music_jit` one central source of truth.
