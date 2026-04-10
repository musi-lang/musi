# Workspace Map

**What**: canonical inventory of crates and their ownership areas.
**Why**: prevents “wrong crate, wrong layer” drift after `crates/` became canonical.
**How**: use this when placing code, splitting modules, or checking dependency direction.
**Where**: design rationale lives in `docs/why/compiler-architecture.md`.

## Canonical Workspace

All current compiler and runtime work lives in `crates/`.

There is no separate rewrite workspace anymore.

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

## Executable Contract And Runtime

- `music_bc`
- `music_assembly`
- `music_emit`
- `musi_vm`
- `musi_rt`

## Service And Project Layer

- `music_session`
- `musi_project`

## Planned

- `music_jit`

## Reading Order

Use this order when orienting yourself:

1. `docs/what/language/syntax.md`
2. `docs/what/runtime/seam-vm.md`
3. `docs/how/runtime/runtime-api.md`
4. `docs/why/compiler-architecture.md`
5. `docs/where/phase-boundaries.md`

## See Also

- `docs/where/phase-boundaries.md`
- `docs/reference/public-api.md`
- `docs/why/compiler-architecture.md`
