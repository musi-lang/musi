# Runtime Boundary

This page explains where runtime behavior stops and host behavior starts.

## Why SEAM And Runtime Are Separate

SEAM is the executable contract. The VM is one executor of that contract.

That split keeps:

- emitted programs transportable
- bytecode validation independent from execution
- runtime implementation details out of compile-time layers

## Why `musi_vm` Is The Embedding Boundary

The stable runtime API belongs at the VM layer because embedding needs:

- loaded program access
- execution
- inspection
- explicit host seams

It does not need compiler internals.

## Why Host-Owned Seams Stay Outside

Some runtime-adjacent behavior should stay host-owned:

- foreign call implementation
- dynamic module source policy
- unhandled host effects

These depend on environment and integration policy, not VM semantics.

Source-backed runtime services belong one layer higher:

- `musi_rt` compiles registered module text into `Program`
- `musi_rt` implements `VmLoader`
- `musi_rt` evaluates syntax by going back through `music_session`
- `musi_rt` owns repo-provided foreign/effect handler registration for the default runtime path
- `musi_native` is the repo-owned default host adapter layer above `musi_vm`
- `musi:*` holds low-level capability roots and `@std/*` wraps them into user-facing library code

## Why This Matters For Design

When deciding placement:

- if it changes SEAM contract, it belongs in SEAM-facing crates
- if it executes SEAM semantics, it belongs in `musi_vm`
- if it composes compiler services with runtime services, it belongs in `musi_rt`
- if it talks to external world or embedding policy, it belongs in the host boundary

## See Also

- `docs/what/runtime/seam-vm.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/how/runtime/runtime-api.md`
