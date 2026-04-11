# Workspace Map

This page lists the crates and packages in the repo.

## Workspace

Rust crates live in `crates/`.

First-party Musi packages live in `packages/`.

## Compiler Core

- `music_base`
- `music_arena`
- `music_names`
- `music_syntax`
- `music_module`
- `music_hir`
- `music_resolve`
- `music_sema`
- `music_ir`

## Executable Contract and Runtime

- `musi_foundation`
- `music_bc`
- `music_assembly`
- `music_emit`
- `musi_vm`
- `musi_rt`

## Service and Project Layer

- `music_session`
- `musi_project`
- `music_tooling`

## Tooling

- `music`
- `musi`

## Native Integration

- `musi_foundation`
- `musi_native`

## First-Party Packages

- `packages/std` (`@std`)
- `musi:*` foundation namespace

## Planned

- `music_jit`

## Reading Order

Use this order when orienting yourself:

1. `docs/what/language/syntax.md`
2. `docs/what/runtime/seam-vm.md`
3. `docs/how/runtime/runtime-api.md`
4. `docs/why/compiler-architecture.md`
5. `docs/where/stack-map.md`
6. `docs/where/phase-boundaries.md`

## See Also

- `docs/where/phase-boundaries.md`
- `docs/where/stack-map.md`
- `docs/reference/public-api.md`
- `docs/why/compiler-architecture.md`
