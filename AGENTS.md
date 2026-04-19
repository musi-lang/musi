# Project Instructions

## Canon

Source of truth:
- Syntax: `grammar/MusiParser.g4`, `grammar/MusiLexer.g4`, `grammar/Musi.abnf`
- Ownership and crate map: `docs/where/workspace-map.md`
- Public API inventory: `docs/reference/public-api.md`

## Scope And Structure

- No placeholders in `crates/`: `todo!()`, `unimplemented!()`, placeholder panics, empty pipelines.
- Documentation and feature-matrix notes describe current implementation behavior. They do not declare language support policy.
- `src/` is production-only. Unit tests live in `module/tests.rs`. Benches live in `benches/` and use Criterion with `bench_` prefix.
- Cargo TOML syntax:
  - workspace-inherited package metadata uses `field.workspace = true`
  - dependency entries use `dep = { workspace = true }`
  - never write dependency entries as `.workspace = true`

## Naming And Imports

- Prefer longform names: `lexer.rs`, `parser.rs`, `syntax`, `sema`.
- Reserve `Syntax*` for syntax trees, not token streams.
- Lexer nouns stay short: `Token`, `TokenKind`, `Trivia`, `Lexer`.
- Keep trivia types in `trivia.rs`.
- Prefer top-level `use` imports and local names in signatures and implementation code.
- Do not write fully qualified paths such as `music_hir::Foo`, `music_arena::Bar`, or `crate::mod::Type` inside function bodies, match arms, or helper signatures just to name ordinary types or variants.
- Absolute singleton uses are only acceptable for real singletons such as `crate::CONST`, `crate::func()`, and macros.
- Do not introduce new macros in this repo.

## Type Alias Rules

- `Vec<Item>` => `ItemList`
- `Box<Item>` => `ItemPtr` or `BoxedItem`
- `Result<T, XError>` => `XResult<T = ()>`
- If one composite type appears 2+ times in one module public surface, alias it.
- Alias names must describe owned concept, not only container shape. Prefer `CallFrameList` over generic names such as `FrameList` when ambiguity exists.

## Decomposition And Ownership

- Do not create God objects, God modules, or catch-all files.
- If one type or file starts owning unrelated responsibilities such as collection, normalization, checking, diagnostics, effect handling, lowering, and declaration validation all at once, split it.
- Prefer small coordinating facades over monolithic structs.
- Organize `music_sema` and later phases by responsibility such as `collect`, `normalize`, `check`, `effects`, `attrs`, `ffi`, not by one giant checker file.
- Helpers belong with owning subsystem. Shared helpers should remove real duplication, not become generic sink modules.

## Diagnostics

- Use `.agents/skills/musi-diagnostics` for diagnostic wording, labels, renderer, or test work.
- Canonical guide: `docs/reference/diagnostics.md`.
- Every diagnostic must come from typed diagnostic enums in its owning crate or phase. Do not construct freeform user-facing diagnostics ad hoc.
- Diagnostic tests outside diagnostic-focused modules must assert enum kind or code, not full message text.
- Diagnostic wording uses **subject-first diagnostic style**:
  - lead with offending item or condition
  - prefer noun-first or adjective-first phrasing such as `unsupported opcode \`foo\`` or `missing export \`bar\``
  - avoid predicate-heavy wording such as `item \`x\` is not supported`
- Diagnostic headlines must identify exact offending source text, symbol, operator, field, variant, import specifier, type, or expected/found pair when known.
- Diagnostic headlines must not use articles: `a`, `an`, `the`.
- Diagnostic headlines should avoid weak linking verbs where practical: `is`, `was`, `but`.
- Diagnostic headlines must not use chained `message1: message2` formatting.
- Hints belong on hint/help lines only when they provide real correction guidance. Do not use hints to restate headline text.
- Primary labels should point at offending token, value, name, or span whenever source location exists.
- Prefer concrete, single-sentence messages with exact offending item named in backticks.

## Phase Boundaries

- Phase DAG has no cycles:
  - `music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir`
  - `music_ir -> music_emit -> music_session`
  - `music_ir -> music_jit -> music_session` is planned
- SEAM transport stays `music_bc -> music_assembly`.
- Project integration lives above compiler/runtime phases in `musi_project` and `music_session`.
- Pre-resolve contracts use `*Env` names, not `*Resolver`.
