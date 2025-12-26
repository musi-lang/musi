# Type Alias Refactoring

Consolidate type aliases into dedicated `types.rs` files across all crates.

---

## Current State

| Crate | Location | Aliases |
|-------|----------|---------|
| `musi_ast` | `lib.rs` | 19 aliases (Ident, ExprPtr, etc.) |
| `musi_basic` | `diagnostic.rs` | 1 alias (DiagnosticList) |
| `musi_lex` | `lexer.rs` | 1 alias (TokenStream) |
| `musi_lsp` | `types.rs` | ✅ Already correct |

---

## Proposed Changes

### musi_ast

#### [NEW] [types.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_ast/src/types.rs)

Move all aliases from `lib.rs`:

```rust
use crate::node::{Attr, AttrArg, Cond, Expr, Field, Pat, Stmt, SumCaseItem, Typ};

pub type Ident = u32;
pub type Idents = Vec<Ident>;
pub type OptIdent = Option<Ident>;

pub type ExprPtr = Box<Expr>;
pub type CondPtr = Box<Cond>;
pub type TypPtr = Box<Typ>;
pub type OptExprPtr = Option<ExprPtr>;
pub type OptExpr = Option<Expr>;
pub type OptTyp = Option<Typ>;

pub type ExprList = Vec<Expr>;
pub type TypList = Vec<Typ>;
pub type PatList = Vec<Pat>;
pub type StmtList = Vec<Stmt>;
pub type SumCaseItemList = Vec<SumCaseItem>;
pub type AttrList = Vec<Attr>;
pub type AttrArgList = Vec<AttrArg>;
pub type FieldList = Vec<Field>;
```

#### [MODIFY] [lib.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_ast/src/lib.rs)

```rust
pub mod node;
pub mod types;
pub mod visitor;

pub use node::*;
pub use types::*;
pub use visitor::*;
```

---

### musi_basic

#### [NEW] [types.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_basic/src/types.rs)

```rust
use crate::diagnostic::Diagnostic;

pub type DiagnosticList = Vec<Diagnostic>;
```

#### [MODIFY] [diagnostic.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_basic/src/diagnostic.rs)

Remove `pub type DiagnosticList = ...` line.

---

### musi_lex

#### [NEW] [types.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lex/src/types.rs)

```rust
use crate::token::Token;
use musi_basic::diagnostic::DiagnosticBag;

pub type TokenStream = (Vec<Token>, DiagnosticBag);
```

#### [MODIFY] [lexer.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lex/src/lexer.rs)

Remove `pub type TokenStream = ...` line, import from `crate::types`.

---

## Verification

```bash
cargo build --workspace
cargo test --workspace
```
