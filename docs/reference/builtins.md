# Builtin Registry

Musi builtins use a hidden catalog, not a public raw `Builtin` module.

- `music_builtin` owns compiler builtin names, foundation module specs, std package file paths, and intrinsic symbols.
- `musi:*` is the Musi-owned built-in module namespace, like `bun:*` in Bun or `node:*` in Node.
- Public `musi:*` modules are reserved for native or compiler-owned APIs such as `musi:core`, `musi:ffi`, `musi:test`, and `musi:syntax`.
- Host-specific primitives for files, text, JSON, encoding, time, randomness, crypto, UUIDs, process state, environment state, formatting, and IO stay behind `@std/*` APIs.
- `Maybe[T]` and `Expect[T, E]` are ordinary library data types. Source sugar `?T`, `E!T`, and `??` lowers through those public types.
- Failure is handled with `match`, `let ... else`, or named helpers, not hidden exception syntax.
- `Bit` is the primitive condition type; `0` is false and `1` is true. There is no truthiness.
- `Bits[N]` is an exact-width bit pattern type. `N` is a compile-time natural number and may be `0`.
- `Word` is the target-sized machine word type. `Word8`, `Word16`, `Word32`, and `Word64` are fixed-width machine word types.
- `Bit` is the primitive one-bit scalar. Type context gives `0` false meaning and `1` true meaning.
- `Unit` has exactly one value, written `()`.
- `Empty` has no values.
- `Type` is the type of type-phase type expressions, not a runtime type object.
- `Unknown` is the opaque top type. Values can be stored or passed as `Unknown`, and useful operations require narrowing.
- `Any` is the dynamic type. Dynamic operations on `Any` are runtime checked.
- Shape conformance uses `|=`. The word `fits` is the reserved readable replacement if `|=` changes.
- Type equivalence uses `~=`. Static casts use `:>`. Runtime type tests use `:?>`.
- `@std/io` uses `write`, `writeLn`, `writeErr`, and `writeErrLn` for output, following the Ada/Pascal/C# `WriteLine` family.
- `Pin[T]` is compiler-owned scoped pin capability created only by `pin value as name in expr`.
- External boundary facts are modeled with `@foreign` metadata, stack effects, explicit unsafe APIs, and ordinary import/export records.

This keeps user code on stdlib APIs while giving future VM work one central source of truth.
