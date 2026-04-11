# Phase Boundaries

This page records the dependency DAG and the layer stops.

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
music_emit -> music_seam
music_seam -> musi_vm
musi_vm -> musi_native
music_session -> musi_rt
musi_vm -> musi_rt
musi_native -> musi_rt
musi_project -> music_session
musi_tooling -> musi_lsp
```

## Ownership Stops

- `music_syntax` stops at parse structure
- `music_resolve` stops at resolved HIR and module identity
- `music_sema` stops at typed and effect-checked semantics
- `music_ir` stops at codegen-facing executable facts
- `music_emit` stops at validated SEAM artifacts
- `musi_vm` starts at loaded `.seam` execution
- `musi_native` owns repo-provided host integration
- `musi_rt` composes compiler services with runtime services
- `music_session` orchestrates phases
- `musi_project` owns package/workspace integration
- `musi_tooling` owns shared external-tool analysis and diagnostics helpers
- `musi_lsp` owns editor-protocol transport over `musi_tooling`

## Stable Rules

- upstream phases do not depend on downstream recovery
- transport does not redefine runtime semantics
- runtime does not own compile-time meaning
- project tooling does not collapse into compiler-core crates
- low-level runtime capabilities surface through `musi:*`; package wrappers stay in `@std/*`
- pre-resolve contracts use `*Env`, not `*Resolver`

## See Also

- `docs/where/workspace-map.md`
- `docs/why/compiler-architecture.md`
- `docs/reference/public-api.md`
