# Musi Compiler - AI Coding Guide

> [!NOTE]
> This guide focuses on architecture and key patterns. See `.agent/rules/` for strict coding standards, behavior protocols, and lint management.

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

Workspace enforces strict lints via `Cargo.toml`. See [`rust-coding-standards.md`](file:///.agent/rules/rust-coding-standards.md) for enforcement rules and documentation guidelines.

**Error Handling Rules**:

- Inevitable failure: `.expect("message")`
- Recoverable: `Result<T, E>`
- Tests only: `.unwrap()`

## Build & Test

```sh
cargo build        # Build all crates
cargo test         # Run unit + integration tests
cargo clippy       # Lint check (must pass clean)
```
