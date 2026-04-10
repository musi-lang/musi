# Runtime Boundary

**What**: rationale for the split between SEAM contract, runtime execution, and host-owned behavior.
**Why**: runtime features stay stable only if the VM boundary is explicit and external-world behavior stays outside it.
**How**: use this when deciding whether behavior belongs in `music_emit`, `musi_vm`, or the embedding host.
**Where**: runtime semantics live in `docs/what/runtime/seam-vm.md`; runtime usage lives in `docs/how/runtime/runtime-api.md`.

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
- syntax evaluation
- unhandled host effects

These depend on environment and integration policy, not core VM semantics.

## Why This Matters For Design

When deciding placement:

- if it changes SEAM contract, it belongs in SEAM-facing crates
- if it executes SEAM semantics, it belongs in `musi_vm`
- if it talks to external world or embedding policy, it belongs in the host boundary

## See Also

- `docs/what/runtime/seam-vm.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/how/runtime/runtime-api.md`
