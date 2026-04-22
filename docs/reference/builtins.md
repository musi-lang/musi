# Builtin registry

Musi builtins use a hidden catalog, not a public raw `Builtin` module.

- `music_builtin` owns compiler-known names, foundation module specs, std package file paths, and intrinsic symbols.
- `musi:core`, `musi:runtime`, and `@std/*` expose stable public APIs over that hidden layer.
- `musi:intrinsics` is registered for compiler-owned internals and is not a user import surface.
- `@musi.known` marks compiler-known source names such as `Type`.
- `Option` and `Result` are ordinary library types. Source sugar `?T`, `E!T`, `??`, and `catch` lowers through those public types.
- `Range[Bound]` is the single canonical range value. It follows Swift's `Range<Bound> where Bound: Comparable` shape: endpoints are values of `Bound`, containment uses comparison evidence, and sequence behavior needs stepping evidence.
- Range endpoint inclusivity lives in the range value, so `a .. b`, `a ..< b`, `a <.. b`, and `a <..< b` share `Range[Bound]`.
- `Pin[T]` is compiler-known scoped pin capability created only by a `pin` action inside an `unsafe` block.
- Intrinsics carry a JIT lowering contract: Cranelift opcode, Cranelift trap, runtime call, VM-only, or unsupported-for-JIT.
- Runtime, filesystem, process, random, time, and target facts remain runtime calls/effects, not raw Cranelift opcodes.
- Native boundary access is modeled through `native`, `@link`, explicit unsafe surfaces, and ordinary import/export records.

This keeps user code on stdlib APIs while giving future `music_jit` one central source of truth.
