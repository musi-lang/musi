# Project Instructions

Source of truth:
- Syntax: `docs/00-syntax.md`, `grammar/Musi.g4` (canonical), `grammar/Musi.abnf` (strict RFC 5234 ABNF spec)
- Ownership + crate map: `docs/09-architecture.md`
- Rewrite tracker: `docs/13-crates_new-rewrite-tracker.md`
- Public API inventory: `docs/12-public-api.md`

Hard rules:
- Edit `crates_new/` only; treat `crates/` as legacy reference-only.
- No placeholders in `crates_new/` (`todo!()`, `unimplemented!()`, placeholder panics, empty pipelines).
- `src/` is production-only; unit tests in `module/tests.rs`; benches in `benches/` using Criterion (`bench_` prefix).
- Naming: longform (`lexer.rs`, `parser.rs`, `sema`, `syntax`); `Syntax*` reserved for syntax trees (not token stream); lexer nouns stay short (`Token`, `TokenKind`, `Trivia`, `Lexer`); trivia types live in `trivia.rs`.
- Imports: avoid absolute paths in expression position; OK for true singletons (`crate::CONST`, `crate::func()`, macros). Do not introduce new macros in this repo.
- Phase DAG (no cycles): `music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir`. Backends are downstream of `music_ir`: `music_ir -> music_emit -> music_session` (SEAM emission) and `music_ir -> music_jit -> music_session` (native/JIT, planned). SEAM transport stays `music_bc -> music_assembly`. Project integration lives above, in `musi_project`/`music_session`. Pre-resolve contracts use `*Env` names, not `*Resolver`.
