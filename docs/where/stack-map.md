# Stack Map

This page maps the first-party crates, packages, and low-level runtime layers.

## First-Party Layers

- `crates/` owns compiler, SEAM, runtime, and service crates
- `packages/std` owns the first-party package surface under `@std/*`
- `musi:*` owns the source-visible foundation surface that everything else builds on

## Package Families

- `@std/*` is portable library surface
- `musi:*` is foundational surface, not STL in disguise
- package names stay normal package names; `@std` is not compiler magic

## Runtime Integration

- `musi_vm` executes SEAM
- `musi_rt` composes compiler services with runtime services
- `musi_native` is the repo-owned host/world integration layer above `musi_vm`

## Placement Rules

- portable pure-language helpers belong in `@std/*`
- foundational source-visible capabilities that other languages usually hide as compiler/package magic belong in `musi:*`
- SEAM execution belongs in `musi_vm`
- source-aware runtime flow belongs in `musi_rt`
- repo-owned host/world integration belongs in `musi_native`

## See Also

- `docs/where/workspace-map.md`
- `docs/why/runtime-boundary.md`
- `docs/what/runtime/foundation-namespace.md`
- `docs/reference/public-api.md`
