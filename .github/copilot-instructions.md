# Copilot Instructions

Use `AGENTS.md` as source of truth. This file mirrors the current high-impact rules for GitHub Copilot.

## Canon

- Syntax sources: `grammar/MusiParser.g4`, `grammar/MusiLexer.g4`, `grammar/Musi.abnf`.
- Ownership/crate map: `docs/where/workspace-map.md`.
- Public API inventory: `docs/reference/public-api.md`.
- Documentation and feature matrices describe current behavior; do not use them to invent support policy.

## Workflow

- Keep changes surgical and production-path focused.
- Run `make lint` before finalizing Rust changes; it uses pinned Rust `1.95.0` and strict Clippy.
- Run `make check` for workspace check coverage.
- Use `cargo fmt --check` or `make fmt` when formatting is required.
- Do not add placeholders in `crates/`: no `todo!()`, `unimplemented!()`, placeholder panics, or empty pipelines.
- Do not introduce new macros.

## Rust Style

- Cargo TOML workspace metadata uses `field.workspace = true`.
- Dependency entries use `dep = { workspace = true }`; never `dep.workspace = true`.
- Prefer top-level `use` imports and local names in signatures and function bodies.
- Avoid fully qualified paths such as `music_hir::Foo`, `music_arena::Bar`, or `crate::mod::Type` for ordinary types/variants inside bodies.
- Absolute singleton paths are acceptable for real singletons such as `crate::CONST`, `crate::func()`, and macros.

## Naming And Layout

- Prefer longform names: `lexer.rs`, `parser.rs`, `syntax`, `sema`.
- Reserve `Syntax*` for syntax trees, not token streams.
- Lexer nouns stay short: `Token`, `TokenKind`, `Trivia`, `Lexer`.
- Keep trivia types in `trivia.rs`.
- `src/` is production-only. Unit tests live in `module/tests.rs`. Benches live in `benches/` and use Criterion with `bench_` prefix.

## Type Alias Rules

- `Vec<Item>` => `ItemList`.
- `Box<Item>` => `ItemPtr` or `BoxedItem`.
- `Result<T, XError>` => `XResult<T = ()>`.
- If one composite type appears 2+ times in one module public surface, alias it.
- Alias names describe owned concepts, not containers. Prefer `CallFrameList` over `FrameList` when ambiguous.

## Decomposition

- Do not create God objects, God modules, or catch-all files.
- Split types/files that own unrelated responsibilities such as collection, normalization, checking, diagnostics, effects, lowering, and declaration validation.
- Prefer small coordinating facades over monolithic structs.
- Organize `music_sema` and later phases by responsibility such as `collect`, `normalize`, `check`, `effects`, `attrs`, `ffi`.
- Helpers belong with the owning subsystem. Shared helpers should remove real duplication, not become sink modules.

## Diagnostics

- Diagnostics must come from typed diagnostic enums in the owning crate/phase.
- Diagnostic tests outside diagnostic-focused modules assert enum kind/code, not full message text.
- Diagnostic wording is subject-first: ``unsupported opcode `foo` `` or ``missing export `bar` ``.
- Diagnostic headlines must not use articles, chained `message1: message2`, or weak linking verbs where avoidable.
- Hints must provide correction guidance; do not restate the headline.
- Primary labels point at the offending token, value, name, or span when source location exists.

## Phase Boundaries

- Phase DAG has no cycles:
  - `music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir`
  - `music_ir -> music_emit -> music_session`
  - `music_ir -> music_jit -> music_session` is planned
- SEAM transport stays `music_bc -> music_assembly`.
- Project integration lives above compiler/runtime phases in `musi_project` and `music_session`.
- Pre-resolve contracts use `*Env` names, not `*Resolver`.
