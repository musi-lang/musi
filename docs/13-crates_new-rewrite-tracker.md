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
- [ ] `music_resolve` (imports + name resolution + syntax→HIR lowering)
- [ ] `music_sema` (type/effect/class checking)
- [ ] `music_ir` (codegen-facing facts)
- [ ] `music_bc` (bytecode contract)
- [ ] `music_assembly` (bytecode codec + validation)
- [ ] `music_emit` (SEAM emission: lowering to bytecode contract)
- [ ] `music_jit` (native/JIT backend consuming `music_ir`)
- [ ] `music_session` (CaaS session: caching + orchestration)
- [ ] `musi_project` (`musi.json` parsing + project integration)

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

- [ ] `opcode.rs` (`Opcode`, `OpcodeFamily` if needed)
- [ ] `instruction.rs` (`Instruction`, `Operand`)
- [ ] `artifact.rs` (`Artifact` + tables)
- [ ] `descriptor/*` (type/effect/foreign/class descriptors)

### `music_session`

- [ ] session state model (sources + options + caches)
- [ ] invalidation strategy (edit-based)
- [ ] entrypoints: parse/check/compile
