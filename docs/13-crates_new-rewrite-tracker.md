# `crates_new/` Rewrite Tracker

This document tracks the canonical compiler rewrite in `crates_new/`.

Rules:

- `crates_new/` is canonical; `crates/` is legacy reference-only.
- Phase dependencies must form a DAG (no circular deps).
- Naming is enterprise-clarity: categorical function names, no unnecessary prefixes, `Syntax*` reserved for syntax tree artifacts, trivia lives in `trivia.rs`.
- No stub modules: once a crate/module exists, it must do real work (e.g. lexers that only emit EOF, parsers that never consume input, or other non-functional skeletons).

## Workspace + Tooling

- [x] Root workspace members are an explicit list (implemented crates only)
- [x] CI builds/tests `crates_new/` only
- [x] `local/` added to `.gitignore` for per-developer checklists
- [x] LOC guard added to CI (fail any file > 2000 LOC)

## Phase Crates (high level)

- [x] `music_base` (Span/Source/Diag foundations)
- [x] `music_arena` (typed ids + arenas + slice storage)
- [x] `music_names` (Symbol/Interner/Ident/KnownSymbols + resolution data)
- [x] `music_syntax` (Token/Trivia/Lexer/Parser/CST+AST views)
- [x] `music_module` (`ImportEnv` + module/specifier model)
- [x] `music_hir` (HIR model)
- [x] `music_resolve` (imports + name resolution + syntax→HIR lowering)
- [x] `music_sema` (type/effect/class checking)
- [x] `music_ir` (codegen-facing facts)
- [x] `music_bc` (bytecode contract)
- [x] `music_assembly` (bytecode codec + validation)
- [x] `music_emit` (SEAM emission: lowering to bytecode contract)
- [ ] `music_jit` (native/JIT backend consuming `music_ir`)
- [x] `musi_vm` (SEAM runtime landing: loader + VM entry/execution path over `.seam`, eager dynamic module load, module-handle export access, handled effects)
- [x] `music_session` (CaaS session: caching + orchestration)
- [x] `musi_project` (`musi.json` parsing + project integration)

## Early Module Checklist (starter granularity)

### `music_base`

- [x] `span.rs` (`Span`, `Spanned`, no absolute paths)
- [x] `source.rs` (`Source`, `SourceId`, `SourceMap`, `SourceMapError` via `thiserror`)
- [x] `diag/*` (model + renderer + message-style validation)
- [x] unit tests in `src/*/tests.rs` (no `#[test]` in production modules)
- [x] Criterion benches in `benches/` (`bench_*` functions)
- [x] `cargo clippy -p music_base --all-targets` clean

### `music_syntax`

- [x] `token.rs` (`Token`, `TokenKind`)
- [x] `trivia.rs` (`Trivia`, `TriviaKind`)
- [x] `lexer.rs` (`Lexer`, `LexedSource`, `LexError*`)
- [x] `LexedSource<'src>` retains source text for parser/tree consumers
- [x] escape validation (`\\xHH`, `\\uXXXX`, `\\uXXXXXX`)
- [x] numeric underscore errors (placement + missing digit)
- [x] fixed-token inventory matches `grammar/Musi.g4` / `grammar/Musi.abnf` (no extra operators; bare `?`/`!` rejected)
- [x] Criterion lexer benches cover mixed + stress cases
- [x] `parser/*` (`Parser`, `ParseError*`, `ParsedSource`, `parse`)
- [x] `tree/*` (`SyntaxTree`, `SyntaxNode*`, `SyntaxToken*`, `SyntaxNodeKind`, CST-backed AST views)

### `music_arena`

- [x] `idx.rs` (`Idx<T>`)
- [x] `arena.rs` (`Arena<T>`, iterator support, typed indexing)
- [x] `slice.rs` (`SliceRange<T>`, `SliceArena<T>`)

### `music_names`

- [x] `symbol.rs` (`Symbol`)
- [x] `interner.rs` (`Interner`)
- [x] `resolution.rs` (`Ident`, `NameSite`, `NameResolution`, `NameBinding*`)
- [x] `known.rs` (`KnownSymbols`)

### `music_bc`

- [x] `opcode.rs` (`Opcode`, `OpcodeFamily`)
- [x] `instruction.rs` (`Instruction`, `Operand`, `Label`)
- [x] `artifact.rs` (`Artifact` + tables + structural validation)
- [x] `descriptor/*` (type/effect/foreign/class/global/method/constant descriptors)

### `music_assembly`

- [x] `binary/*` (SEAM binary encode/decode + validation)
- [x] `text/*` (SEAM text format/parse + validation)
- [x] shared `AssemblyError` boundary
- [x] text and binary roundtrip coverage

### `music_ir`

- [x] lowered module facts (`IrModule`)
- [x] callable/data/foreign fact extraction
- [x] exported effect/class/instance metadata lowering
- [x] sema-surface validation before lowering

### `music_session`

- [x] session state model (sources + options + caches)
- [x] invalidation strategy (edit-based)
- [x] entrypoints: parse/check/compile

### `musi_project`

- [x] typed `musi.json` manifest model
- [x] workspace-aware project loading
- [x] lockfile loading and frozen-lock validation
- [x] registry-to-cache package resolution
- [x] package-aware compilation through `music_session`
