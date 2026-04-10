# Stack Map

This page maps the first-party crates, packages, and low-level runtime layers.

## First-Party Layers

- `crates/` owns compiler, SEAM, runtime, and service crates
- `packages/std` owns the first-party package surface under `@std/*`
- `musi:*` owns compiler/runtime intrinsic capability surface

## Package Families

- `@std/*` is portable library surface
- `musi:*` is low-level intrinsic surface
- package names stay normal package names; `@std` is not compiler magic

## Runtime Integration

- `musi_vm` executes SEAM
- `musi_rt` composes compiler services with runtime services
- `musi_native` is the repo-owned default host adapter layer above `musi_vm`

## Placement Rules

- portable pure-language helpers belong in `@std/*`
- low-level host/runtime capabilities belong in `musi:*`
- SEAM execution belongs in `musi_vm`
- source-aware runtime flow belongs in `musi_rt`
- repo-owned host/world integration belongs in `musi_native`

## See Also

- `docs/where/workspace-map.md`
- `docs/why/runtime-boundary.md`
- `docs/reference/public-api.md`
