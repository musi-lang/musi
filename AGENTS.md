## Project Instructions (compressed)

### Pointers (source of truth)

- Musi surface syntax: `docs/00-syntax.md` and `grammar.abnf`
- Compiler ownership + crate map: `docs/09-architecture.md`
- Rewrite tracking (checkboxes): `docs/13-crates_new-rewrite-tracker.md`
- Public Rust API inventory: `docs/12-public-api.md`

### Canonical workspace

- Edit `crates_new/` only.
- Treat `crates/` as legacy reference-only.
- No “stubs”: placeholder implementations (EOF-only lexers, empty parsers, TODO pipelines) are not acceptable once a crate/module exists—either implement it fully or delete it.

### Rust layout + tests (HARD)

- `src/` is production code only (no `#[test]` functions).
- Unit tests live in `module/tests.rs` and are included via `#[cfg(test)] mod tests;`.
- Integration/e2e tests live in `tests/` alongside `src/`.
- Benchmarks live in `benches/` and use Criterion; benchmark functions use `bench_` prefix.
- No Rust file may exceed 2000 LOC (including tests and benches).

### Naming + imports (HARD)

- Prefer longform: `sema`, `syntax`, `lexer.rs` (struct `Lexer`), `parser.rs` (struct `Parser`).
- Crate/module path carries the domain: prefer `music_bc::Opcode` over `BytecodeOpcode`.
- Reserve `Syntax*` for syntax tree artifacts (`SyntaxTree`, `SyntaxNodeKind`, …), not for the token stream.
- Tokenization uses unprefixed nouns: `Token`, `TokenKind`, `Trivia`, `Lexer`, `Parser`.
- Avoid absolute paths in expression position; prefer `use` imports. Absolute paths are OK for singletons (`crate::CONST`, `crate::func()`, macros).

### Phase DAG (HARD)

Dependencies must form a DAG. The intended compiler flow is:

`music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir -> music_bc -> music_assembly -> music_codegen -> music_session`

### Phase Ownership (no cycles)

- Each compiler phase owns one transformation and depends only on earlier representations.
- Service/orchestration concerns (caching, config, filesystem/project integration) live only in the session/service layer.
- Pre-resolve module lookup contracts use neutral naming (`ImportEnv`/`ModuleEnv`), not `*Resolver`.

### Naming Conventions (enterprise clarity)

- **Domain by path**: prefer `music_bc::Opcode`, `music_syntax::Token`, `music_sema::ErrorKind` over redundant domain prefixes.
- **`Syntax*` is tree/AST only**: `SyntaxTree`, `SyntaxNodeKind`, `SyntaxToken` are syntax tree artifacts, not lexer token stream artifacts.
- **Tokenizer nouns stay short**: `Token`, `TokenKind`, `Trivia`, `TriviaKind`, `Lexer`, `Parser`, `ParseCtx`.
- **Trivia is its own module**: `trivia.rs` defines trivia types; it does not live in `token.rs`.
- **File name matches primary type**: `lexer.rs`→`Lexer`, `parser.rs`→`Parser`, `opcode.rs`→`Opcode`.
- **Function naming is categorical**: `emit_case_expr`, `lower_import_expr`, `check_record_update_expr`.
- **Avoid absolute paths in expression position** except true singletons (`crate::CONST`, `crate::func()`, macros).

### “Stubs” Policy For The Rewrite

- New `crates_new/` crates may start as minimal compileable skeletons, but must not contain `todo!()`, `unimplemented!()`, or placeholder panics.
