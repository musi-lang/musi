# Workspace map

This workspace splits language pipeline, runtime, tooling, and user-facing binaries into small crates.

## Compiler pipeline

- `music_base` — shared source spans, diagnostics, source maps
- `music_builtin` — hidden builtin type, intrinsic, std/foundation registry
- `music_names` — symbol interning and compiler name tables
- `music_syntax` — lexer, parser, syntax tree
- `music_module` — module keys, import maps, module syntax helpers
- `music_resolve` — name and import resolution
- `music_sema` — semantic checking, exported surface, effect rows
- `music_ir` — IR model and diagnostics
- `music_ir_lower` — sema-to-IR lowering
- `music_emit` — SEAM emission from IR
- `music_session` — end-to-end compile orchestration and caches

## Runtime and host

- `music_seam` — SEAM artifact, SEAM HIL, lowered `.seam` text, binary, opcodes
- `musi_vm` — VM program loading, values, execution
- `musi_native_ffi` — libffi-backed native call bridge
- `musi_native` — host effect and native dispatch
- `musi_rt` — embeddable runtime wrapper around session + VM + native host
- `musi_foundation` — compiler-owned `musi:*` modules

## Project and tooling

- `musi_project` — package manifests, workspace graph, registry, project compile entrypoints
- `musi_fmt` — Musi source formatting options, source formatting, and path formatting
- `musi_tooling` — CLI-facing diagnostics, hover, direct tooling helpers
- `musi_lsp` — language server

## User-facing binaries

- `music` — direct file and artifact lane
- `musi` — package/workspace lane

## Supporting crates

- `music_arena` — arena and slice-index utilities
- `music_hir` — HIR data model
- `music_term` — syntax and type term helpers

## Dependency direction

Primary phase DAG:

- `music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir`
- `music_ir + music_sema -> music_ir_lower -> music_session`
- `music_ir -> music_emit -> music_session`
- `music_seam -> musi_vm -> musi_native -> musi_rt`
- `musi_foundation`, `musi_project`, and `musi_tooling` sit above core compiler/runtime crates
