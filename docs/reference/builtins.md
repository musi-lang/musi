# Builtin registry

Musi builtins use a hidden catalog, not a public raw `Builtin` module.

- `music_builtin` owns compiler-known type names, foundation module specs, std package file paths, and intrinsic symbols.
- `musi:core`, `musi:runtime`, and `@std/*` expose stable public APIs over that hidden layer.
- `musi:intrinsics` is registered for compiler-owned internals and is not intended as a user import surface.
- Intrinsics carry a JIT lowering contract now: Cranelift opcode, Cranelift trap, runtime call, VM-only, or unsupported-for-JIT.
- Runtime, filesystem, process, random, time, and target facts remain runtime calls/effects, not raw Cranelift opcodes.

This keeps user code on stdlib APIs while giving future `music_jit` one central source of truth.
