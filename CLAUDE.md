# Musi Compiler — Claude Code Instructions

## Project overview

Musi-next is a fresh rewrite of the Musi compiler. `docs/` is the source of truth for the language design. `crates_old/` contains the legacy implementation (reference only, do not modify). The root `grammar.ebnf` is legacy and will be replaced.

## Cargo workspace conventions

### Package metadata vs dependencies

Package metadata fields use **dot syntax**:

```toml
version.workspace = true
edition.workspace = true
license.workspace = true
readme.workspace = true
```

Dependency entries use **table syntax**:

```toml
[dependencies]
intaglio = { workspace = true }
memchr = { workspace = true }
musi_shared = { path = "../musi_shared" }
```

### Workspace lints (strict)

The workspace has aggressive clippy and rustc lints. Key constraints:

- `unused_results = "forbid"` — always bind or `let _ =` discarded results
- `as_conversions = "deny"` — use `u32::try_from(x).expect(...)` not `x as u32`
- `string_slice = "deny"` — use `.get(range).expect(...)` not `&s[range]` on `str`
- `panic = "deny"` — no `panic!()` macro (but `expect`/`assert` are fine)

## Code conventions

### Naming

- Keywords: `KwLet`, `KwClass`, etc. (Kw-prefixed)
- Multi-char symbols: `DotDotDot`, `ColonEq`, `LtDash`, `DashGt`, `TildeGt`, `PipeGt`
- Rune literals (not "char"): `RuneLit` for `'x'`
- Diagnostic messages: lowercase, no articles, no contractions

### Test structure

- Test files live in sibling directories: `foo.rs` → `foo/tests.rs` via `#[cfg(test)] mod tests;`
- Test naming: `test_{feature}_{scenario}_{expected_result}`
- Arrange-Act-Assert pattern

### Lexer design

- Combinators: `peek`, `peek_at`, `advance`, `advance_by`, `eat_while`, `at`, `expect`, `expect_if`
- `lex_punct` is a single `match (b0, b1, b2)` table — maximal munch by construction (3-char first, then 2-char, then 1-char). No separate per-operator methods, no fallback concept.
- Predicate helpers as `const fn` for reusable checks (`is_exponent`, `is_sign`, `is_hex_digit`, etc.)
- `LexError` enum for all diagnostic messages — no ad-hoc strings
- F-strings use Head/Middle/Tail model with mode stack for brace depth tracking
- Comments are trivia-attached to tokens (rust-analyzer style)
