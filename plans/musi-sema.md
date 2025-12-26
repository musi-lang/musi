# Musi Sema Module

Semantic analysis with gradual typing and local HM inference.

---

## Prerequisites

- [Arena AST Refactor](file:///Users/krystian/CodeProjects/musi/plans/arena-ast-refactor.md) (NodeId on AST nodes)
- [Type Alias Refactor](file:///Users/krystian/CodeProjects/musi/plans/type-alias-refactor.md) (optional, cleanup)

---

## Architecture

```
musi_sema/
├── lib.rs              # bind() API
├── types.rs            # Type aliases
├── typ_repr.rs         # TypRepr, TypReprKind
├── bound.rs            # BoundExpr, BoundStmt (semantic nodes)
├── symbol.rs           # SymbolTable, Symbol, SymbolId
├── infer.rs            # Type inference (unification)
├── binder.rs           # AST → Semantic tree
├── builtins.rs         # Builtin types
└── tests.rs
```

---

## Key Design Decisions

### Gradual Typing

- `Any` is top type (like TypeScript)
- `Any ~ T` for all `T` (consistency relation)
- `DynCast` nodes inserted at type boundaries

### Semantic Model (NodeId-indexed)

```rust
pub struct SemanticModel {
    types: Vec<Option<TypRepr>>,     // indexed by Expr.id
    symbols: Vec<Option<SymbolId>>,  // indexed by ident's Expr.id
}
```

### No Tree Duplication

- BoundExpr references original AST via `NodeId`
- Semantic info stored in side tables
- LSP queries: `model.type_of(node.id)`

---

## TypRepr (Semantic Type)

```rust
pub enum TypReprKind {
    // Primitives
    Int(IntWidth), Nat(IntWidth), Float(FloatWidth),
    Bool, Rune, String, Unit, Never,

    // Gradual
    Any, Unknown,

    // Compound
    Tuple(TypReprList), Array { elem, size }, Ptr, Optional, Fn,

    // User-defined
    Named { symbol: SymbolId, args },

    // Inference
    Var(TyVarId), Error,
}
```

---

## Implementation Order

1. `typ_repr.rs` - Type representation
2. `symbol.rs` - Symbol table
3. `builtins.rs` - Builtin types
4. `infer.rs` - Unification engine
5. `bound.rs` - Semantic nodes
6. `binder.rs` - Main binder
7. `lib.rs` - Wire up
8. `tests.rs`

---

## Verification

```bash
cargo test -p musi_sema
```

---

## Open Questions

1. **Default integer**: `42` → `Int32` or polymorphic?
2. **Operator overloading**: traits/typeclasses?
3. **Exhaustiveness**: require exhaustive `match`?
