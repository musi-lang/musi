# Arena AST Refactoring

Add arena allocation and `NodeId` to AST nodes for better semantic analysis and LSP tooling.

---

## Motivation

1. **Direct semantic lookup**: `model.type_of(node_id)` instead of `HashMap<Span, TypRepr>`
2. **Stable identity**: NodeId is guaranteed unique, spans could collide in edge cases
3. **LSP-friendly**: Hover/go-to-definition can directly index into semantic model
4. **Memory layout**: Arena keeps nodes contiguous, better cache locality

---

## Proposed Changes

### musi_basic

#### [NEW] [arena.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_basic/src/arena.rs)

Simple typed arena:

```rust
pub struct Arena<T> {
    nodes: Vec<T>,
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, node: T) -> NodeId<T> { ... }
    pub fn get(&self, id: NodeId<T>) -> &T { ... }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct NodeId<T> {
    index: u32,
    _marker: PhantomData<T>,
}
```

---

### musi_ast

#### [MODIFY] All node types

Add `NodeId` field alongside `Span`:

```rust
pub struct Expr {
    pub id: NodeId<Expr>,   // NEW: unique identity
    pub kind: ExprKind,
    pub span: Span,
}
```

#### [MODIFY] Parser

Allocate nodes via arena:

```rust
pub struct Parser<'a> {
    // ...
    arena: &'a mut Arena<Expr>,
}
```

---

### musi_sema

Semantic model indexes by `NodeId`:

```rust
pub struct SemanticModel {
    types: Vec<Option<TypRepr>>,     // indexed by NodeId
    symbols: Vec<Option<SymbolId>>,  // indexed by NodeId
}

impl SemanticModel {
    pub fn type_of(&self, id: NodeId<Expr>) -> Option<&TypRepr> {
        self.types.get(id.index())?.as_ref()
    }
}
```

---

## Migration Strategy

1. Add `Arena` and `NodeId` to `musi_basic`
2. Add `id: NodeId<Self>` to `Expr`, `Stmt`, `Pat`, `Typ`
3. Update parser to use arena allocation
4. Update tests
5. Semantic model uses NodeId indexing

---

## Verification

```bash
cargo build --workspace
cargo test --workspace
```
