# Phase Boundaries

**What**: dependency DAG and layer edges across compiler, SEAM, runtime, and tooling crates.
**Why**: keeps meaning, lowering, transport, runtime, and project workflow from collapsing into each other.
**How**: use this when adding dependencies, moving APIs, or deciding where one feature belongs.
**Where**: crate ownership lives in `docs/where/workspace-map.md`.

## Compiler DAG

```text
music_base
  -> music_names
  -> music_syntax
  -> music_module
  -> music_resolve
  -> music_sema
  -> music_ir
```

## Downstream Boundaries

```text
music_ir -> music_emit -> music_session
music_ir -> music_jit -> music_session   (planned)
music_bc -> music_assembly -> musi_vm
music_session -> musi_rt
musi_vm -> musi_rt
musi_project -> music_session
```

## Ownership Stops

- `music_syntax` stops at parse structure
- `music_resolve` stops at resolved HIR and module identity
- `music_sema` stops at typed and effect-checked semantics
- `music_ir` stops at codegen-facing executable facts
- `music_emit` stops at validated SEAM artifacts
- `musi_vm` starts at loaded `.seam` execution
- `musi_rt` composes compiler services with runtime services
- `music_session` orchestrates phases
- `musi_project` owns package/workspace integration

## Stable Rules

- upstream phases do not depend on downstream recovery
- transport does not redefine runtime semantics
- runtime does not own compile-time meaning
- project tooling does not collapse into compiler-core crates
- pre-resolve contracts use `*Env`, not `*Resolver`

## See Also

- `docs/where/workspace-map.md`
- `docs/why/compiler-architecture.md`
- `docs/reference/public-api.md`
