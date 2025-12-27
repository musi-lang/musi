# Musi Compiler - AI Coding Guide

## Project Overview

Musi is a hand-written compiler for the Musi programming language (`.ms` files).

**Crate Architecture** (dependency order):

```text
musi_basic → musi_lex → musi_ast → musi_parse → musi_sema → musi_codegen
                                  ↓
                               musi_lsp
                               musi_drive (CLI)
                               musi (REPL)
```

## Source of Truth

- **`grammar.ebnf`**: Human-authored grammar specification. NEVER auto-correct or optimize without asking.
- **Naming in grammar**: `aux_` = no AST, `expr_`/`pat_`/`ty_expr_`/`stmt_` = create AST nodes, `prec_` = parser precedence.

## Code Patterns

### Arena Allocation

AST nodes use typed arena IDs (`NodeId<T>`):

```rust
pub type ExprId = NodeId<Expr>;

AstArena.alloc_expr(kind: ExprKind, span: Span) -> ExprId
```

Lookup: `arena.exprs[id]`.

### Pratt Parsing

The parser uses Pratt parsing for operator precedence. `Prec` enum defines precedence levels (1-17), with `infix()`, `prefix()`, `postfix()` returning binding powers. Right-associative ops (e.g., `**`, `<-`) have `left_bp < right_bp`.

### Public Re-exports

Crates use internal `types.rs` module + `pub use types::*` in `lib.rs` for clean API.

## Lint Compliance

Workspace enforces strict lints via `Cargo.toml`. **Do NOT suppress with `#[allow(...)]` unless unavoidable** (e.g., function genuinely cannot be split into smaller functions).

**Attribute Guidelines**: Only add `#[must_use]` or `#[inline]` when `cargo clippy` suggests them.

### Error Handling Rules

| Context | Use | Example |
|---------|-----|---------|
| Inevitable failure | `.expect("message")` | Arena overflow |
| Recoverable error | `Result<T, E>` | Parse errors |
| Tests only | `.unwrap()` | `tests.rs`, `@/tests/` |
| Critical path access | `.get()` + handle | Bounds checking |

### Key Lint Rationale

- `indexing_slicing = "deny"` (commented): Use `.get()` in critical paths, direct indexing when bounds are provable.
- `unwrap_used = "deny"` (commented): `.expect()` in prod with descriptive message, `.unwrap()` only in test files.
- `panic = "deny"`: No panics except via `.expect()` for invariant violations.

## Build & Test

```sh
cargo build        # Build all crates
cargo test         # Run unit + integration tests
cargo clippy       # Lint check (must pass clean)
```

## File Structure Conventions

- **Unit tests**: `foo.rs` + `foo/tests.rs` sibling.
- **Integration tests**: `/tests/` directory.
- **Function ordering in `impl`**: Constructors → Public API → Internal helpers → Navigation (`peek`/`advance`).

## Borrow Checker Guidance

Avoid `map_or_else`/`and_then` with closures capturing `&mut self`. Prefer `match`/`if let` for clearer lifetime boundaries.
