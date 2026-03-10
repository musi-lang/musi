# Musi Compiler — Claude Code Instructions

## Project overview

Musi-next is a fresh rewrite of the Musi compiler. `docs/` is the source of truth for the language design. `crates_old/` contains the legacy implementation (reference only, do not modify). The root `grammar.ebnf` is legacy and will be replaced.

## Crate naming: `musi_*` vs `music_*`

The project splits into two families, analogous to `java`/`javac`, `cargo`/`rustc`, `sbt`/`scalac`:

- **`musi_*`** — the runtime and standard library crates
- **`music_*`** — the compiler crates (`music` = `musi` + `c`, where `c` stands for compiler)

The name has nothing to do with actual music. Do not rename, conflate, or explain these as music-related.

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

- `unsafe_code = "deny"` — no unsafe code by default. Crates that genuinely require unsafe (e.g. FFI via `musi_std`) opt in with `#![allow(unsafe_code)]` at the crate root
- `unused_results = "forbid"` — always bind or `let _ =` discarded results
- `as_conversions = "deny"` — use `u32::try_from(x).expect(...)` not `x as u32`
- `string_slice = "deny"` — use `.get(range).expect(...)` not `&s[range]` on `str`
- `panic = "deny"` — no `panic!()` macro (but `expect`/`assert` are fine)

### Full lint check command

```bash
cargo fmt --all && cargo check && cargo check --tests && cargo clippy && cargo clippy --tests
```

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

### Error handling

Each compiler crate defines a crate-level `error.rs` module with a single `thiserror`-derived enum:

- `music_lex` → `LexError` in `crates/music_lex/src/error.rs`
- `music_parse` → `ParseError` in `crates/music_parse/src/error.rs`

Pattern:
- One enum per crate, named `{Phase}Error` (`LexError`, `ParseError`, `ResolveError`, …)
- Every variant uses `#[error("...")]` — no ad-hoc string construction
- All error enums implement `IntoDiagnostic` (from `music_shared`), which bakes in `severity()`
- Errors are reported via `DiagnosticBag::report(&error, span, file_id)` — never call `.to_string()` manually
- Visibility: `pub` in a private `mod error` (effectively crate-private; avoids `clippy::redundant_pub_crate`)
- `thiserror` for the derive; no `anyhow` in compiler crates

Diagnostic message style:
- Lowercase, no leading articles ("a"/"the"), no contractions
- No internal jargon — use terms an end user would recognise:
  - "interpolated string" not "f-string"
  - "backtick-quoted identifier" not "escaped identifier"
- `TokenKind` implements `Display` — fixed-text tokens produce `'let'`, `'+'`; variable tokens produce `identifier`, `integer literal`. Use this in error templates, never store pre-formatted `&'static str` descriptions

### Comments

- No `// ── Section ──...` ASCII dividers in source code. Module structure and function names provide organization. Exception: test files and large enum definitions where section grouping aids readability.
- No doc comments that restate the field/variant name (e.g., `/// The value.` on a field called `value`). Only add doc comments that provide information beyond what the name already conveys.
- No narrational comments describing stack effects, internal arithmetic, or step-by-step play-by-play (e.g., `// net 0: pops obj, pushes val`). The code is the source of truth.
- No `NEW:` or other historical/temporal markers in comments.

## STL stub files (`std/`)

The `std/` directory at project root contains `.ms` stub files — Musi source declarations with no implementations. These define the public API surface of the standard library. They are organized into subdirectories:

```
std/
  core/       option.ms, result.ms, ordering.ms
  typeclasses/ add.ms, eq.ms, into.ms, iterable.ms, ord.ms, propagate.ms, show.ms
  collections/ list.ms, map.ms, set.ms
  io/         file.ms, path.ms, write.ms
  math/       index.ms, random.ms
  text/       fmt.ms, parse.ms
  testing/    assert.ms, bench.ms
  ffi/        c.ms
  concurrency/ channel.ms, task.ms
```

### Quantification rules in `.ms` files

This is the single most important rule for writing correct Musi type signatures:

**`forall` is only used in `:=` expression bindings** — where a type variable must be explicitly introduced:
```
let Option := forall 'T -> Some of 'T + None
let Result := forall 'T, 'E -> Ok of 'T + Err of 'E
let List   := forall 'T -> Nil + Cons of ('T, List of 'T)
```

**`:` type annotations use bare `'T` — implicitly universally quantified**, no `forall`:
```
let assert_eq : ('T, 'T) -> ()         -- correct
let assert_eq : forall 'T -> ('T, 'T) -> ()  -- WRONG: forall not needed here
```

**`class ... over 'T`** — `over` introduces the type param, no `forall`:
```
class Eq over 'T (
    let (=)(a: 'T, b: 'T) : Bool
)
```

**`where`** — constraint syntax on `forall`/`class`/`given`:
```
forall 'T where 'T <: Ord -> ...
```

### Lang items

`#[lang := "..."]` marks STL types as compiler-known. Enables syntactic sugar:
- `#[lang := "option"]` on `Option` — enables `?T` sugar for `Option of T`
- `#[lang := "result"]` on `Result` — enables `try`/`?` sugar
- `#[lang := "propagate"]` on `Propagate` — enables `try` desugaring

### FFI type stubs (`std/ffi/c.ms`)

Defines C-interop type aliases: `CInt`, `CUInt`, `CChar`, `CSize`, `CDouble`. These will eventually move behind `import "musi:ffi"` once the module system is complete. No `CString` stub yet.

### Effect and import conventions

- `~> T under { IO }` — effectful function returning `T` with `IO` effect
- `~> T under { Async }` — async computation
- `~> T under { State }` — stateful computation
- `@musi/core` = implicit prelude (always available)
- `@musi/*` = explicit stdlib import
- No `@` prefix = relative path import

## Key Musi differences from C/Rust

Do not assume C/Rust conventions. Musi has its own idioms:

| Musi | C/Rust | Difference |
|---|---|---|
| `#[entrypoint]` attribute | `fn main()` | No reserved function name. Entry point is attribute-based. Error on duplicates. |
| `:=` | `=` | Binding operator is `:=`, not `=`. `=` is equality. |
| `() -> body` | `fn() -> body` | Function literals use paren params + arrow. `fn` is NOT a keyword for literals. |
| `(expr if cond)` | `if cond { expr }` | Piecewise expressions, not if/else. |
| `and`/`or`/`not`/`xor` | `&&`/`\|\|`/`!` | Keyword logical ops. Also type-directed for bitwise. |
| `/=` | `!=` | Inequality operator. |
| `<-` | `=` (assignment) | Mutation uses `<-`, not `=`. |
| `#[...]` | `#[...]` (Rust) | Attribute syntax. `#[` is a single compound token (`HashLBracket`). |
| `(a; b; c)` | `{ a; b; c }` | Parentheses delimit blocks/sequences, not braces. |
