# Musi Sema Module

Semantic analysis with gradual typing and local HM inference.

---

## Architecture

```text
musi_sema/
├── lib.rs              # bind() API
├── types.rs            # Type aliases
├── ty_repr.rs          # TyRepr, TyReprKind
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
    types: Vec<Option<TyRepr>>,     // indexed by Expr.id
    symbols: Vec<Option<SymbolId>>,  // indexed by ident's Expr.id
}
```

### No Tree Duplication

- BoundExpr references original AST via `NodeId`
- Semantic info stored in side tables
- LSP queries: `model.type_of(node.id)`

---

## TyRepr (Semantic Type)

```rust
pub enum TyReprKind {
    // Primitives
    Int(IntWidth), Nat(IntWidth), Float(BinWidth),
    Bool, Rune, String, Unit, Never,

    // Gradual
    Any, Unknown,

    // Compound
    Tuple(TyReprs), Array { elem, size }, Ptr, Optional, Fn,

    // User-defined
    Named { symbol: SymbolId, args },

    // Inference
    Var(TyVarId), Error,
}
```

---

## Implementation Order

1. `ty_repr.rs` - Type representation
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

## Design Decisions (Resolved)

### Numeric Literal Inference

- **Non-negative integers** (`42`) → `Nat` (natural number)
- **Negative integers** (`-42`) → `Int`
- **Decimal literals** (`3.14`) → `Float` (binary float)

### Operator Overloading

Currently **not supported**. No traits or typeclasses exist.

---

## Future: Proposed Type Class Syntax

Based on existing Musi patterns (`record` uses `;`, `choice` uses `,`):

```musi
// Class definition (semicolon-separated, like record)
val Eq := class Eq[T] {
  eq: (T, T) -> Bool;
  neq: (T, T) -> Bool
};

// Instance (semicolon-separated)
instance Eq[Int32] {
  eq := fn(a, b) { a = b };
  neq := fn(a, b) { a /= b }
};

// Constrained type parameters
val contains := fn[T: Eq](list: []T, elem: T): Bool {
  for x in list {
    if Eq.eq(x, elem) { return true; }
  };
  false
};
```

### Proposed Grammar Extensions

```ebnf
expr_class_def = [aux_attr], [aux_modifiers], "class", ident, ty_expr_params, "{", aux_field_list_semi, "}";

expr_instance = "instance", ty_expr_app, "{", aux_field_list_semi, "}";

(* Constrained type params - extends existing ty_expr_params *)
aux_ident_param = ident, [":", ty_expr];
aux_ident_param_list = aux_ident_param, {",", aux_ident_param}, [","];
ty_expr_params = "[", [aux_ident_param_list], "]";
```

### Match Exhaustiveness

**Configurable** via `compilerOptions`. The semantic analyzer will support both:

- Exhaustive mode (error on non-exhaustive matches)
- Non-exhaustive mode (warning or silent)
