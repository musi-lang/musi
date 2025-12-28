# Semantic Analysis Architecture Overhaul

## Current Problems

- **Monolithic binder.rs** (813 lines, 43 functions) handles everything
- No separation between global declarations and local inference
- Scope management is ad-hoc (push/pop scattered throughout)
- Type unification is reactive, not structured in phases

## Goals

1. **Two-phase binding**: Global declarations → Local inference
2. **Clean module boundaries** per concern
3. **Better error recovery** via phase separation
4. **Gradual typing** support with explicit boundaries

---

## Module Structure ✅ IMPLEMENTED

```text
musi_sema/
├── lib.rs                    # Public API: bind()
├── types.rs                  # Re-exports
├── binder.rs                 # 88-line orchestrator (was 825 lines)
│
├── phase1/                   # Phase 1: Global declarations
│   ├── mod.rs
│   ├── collector.rs          # Collect type/variant names (hoisted)
│   └── resolver.rs           # Resolve type references
│
├── phase2/                   # Phase 2: Local inference
│   ├── mod.rs
│   ├── ctx.rs                # Shared binding context (BindCtx)
│   ├── expr.rs               # Expression binding + inference
│   ├── pat.rs                # Pattern binding
│   ├── stmt.rs               # Statement binding
│   └── ty.rs                 # Type expression resolution
│
├── symbol.rs                 # Symbol table
├── unifier.rs                # Type unification (union-find)
├── ty_repr.rs                # Type representation
├── semantic.rs               # Semantic model for LSP
├── builtins.rs               # Builtin types
└── error.rs                  # Errors
```

---

## Phase 1: Global Declaration Collection

**Input**: AST
**Output**: SymbolTable with type/fn/alias declarations

Collects:

- `record` definitions → Type symbols
- `choice` definitions → Type + Variant symbols
- `alias` definitions → Type symbols
- `fn` signatures → Function symbols (body not processed)
- `extern` blocks → External function symbols

Does NOT process:

- Function bodies
- Expression types
- Pattern bindings

---

## Phase 2: Local Inference

**Input**: AST + Phase 1 SymbolTable
**Output**: Fully typed SemanticModel

For each function body:

1. Create local scope
2. Bind parameters (with declared types or fresh vars)
3. Infer expression types using HM algorithm
4. Unify return type with body type
5. Finalize type variables

Key: Each function is independent - can parallelize later.

---

## Migration Path ✅ COMPLETED

1. ✅ **Extract pattern binding** → `phase2/pat.rs`
2. ✅ **Extract expression binding** → `phase2/expr.rs`
3. ✅ **Add global collector** → `phase1/collector.rs`
4. ✅ **Split entry point** into two phases
5. ✅ **Move tests** to `binder/tests.rs`

---

## Gradual Typing (Any)

Musi supports a **spectrum** from full static to full dynamic:

| Mode | How | Example |
|------|-----|---------|
| **Full static** | All types declared or inferred | `val x: Int := 42` |
| **Partial** | Some types, inference fills gaps | `val x := foo()` where foo returns known type |
| **Full dynamic** | Everything is `Any` | `val x := ...` where inference fails → `Any` |

### Any Semantics

- `Any` is a **gradual typing escape hatch** - compatible with all types
- `Never` is the **bottom type** (uninhabited, for diverging expressions like `return`)
- Unification: `unify(Any, T)` → succeeds, produces `T` (or `Any` if both unknown)
- Assignment: `Any` can be assigned to/from anything without error
- Operations: Any operation on `Any` returns `Any`

```musi
val x := get_dynamic_value();   // x: Any (inference fails)
val y := x + 1;                 // y: Any (Any + Int → Any)
val z: Int := y;                // OK at compile (runtime may fail)
```

### Inference Fallback Rule

When type variable remains unresolved after finalization:

1. If has no constraints → default to `Any`
2. If has partial constraints → use most general type

```rust
// In unifier.rs finalize()
fn finalize(&mut self, ty: &TyRepr) -> TyRepr {
    match &ty.kind {
        TyReprKind::Var(id) => {
            let resolved = self.find(*id);
            if resolved.is_unbound() {
                TyRepr::any()  // ← Fallback to Any
            } else {
                self.finalize(&resolved)
            }
        }
        // ...
    }
}
```

### Gradual Guarantee

- **Static part**: Type errors caught at compile time where types are known
- **Dynamic part**: Deferred to runtime where `Any` is involved
- **Boundary**: Explicit casts or runtime checks at static/dynamic boundary

### Configuration

- `--strict` - Disallow Any fallback, require all types (confirmed `compilerOptions` flag)
- `--warn-any` - Warn when Any is inferred (conceptual)
- `--allow-any` - Default mode, allow Any (conceptual)

> NOTE: `--strict` is a confirmed TS-like flag. Other options are conceptual and likely won't make it into CLI, or will be renamed. Unknown at the moment.

---

---

## Algorithms & Data Structures

### Unification: Union-Find (Current)

We use **union-find** (disjoint-set) for type unification:

- `unify(a, b)` → O(α(n)) amortized (inverse Ackermann, effectively constant)
- `find(a)` → O(α(n)) with path compression
- No explicit substitution maps needed

**Current implementation**: `unifier.rs` already uses this approach with `TyRepr` containing type vars.

### Why NOT Substitution

Substitution-based unification:

- Build map `{α → Int, β → α}`
- Apply substitution to all types: O(n) per type
- Transitive closure needed: O(n²) worst case

Union-find avoids this by representing equivalence classes directly.

### Constraint-Based Inference (Phase 2)

Instead of immediate unification during binding:

1. **Collect constraints**: `α ~ β`, `α ~ Int → β`
2. **Solve constraints**: Run unification on constraint set
3. **Finalize**: Replace remaining vars with defaults

Benefits:

- Better error messages (show conflicting constraints)
- Can defer solving until more info available
- Easier to implement gradual typing boundaries

### Type Representation

```rust
enum TyReprKind {
    Var(VarId),         // Unification variable
    Named(SymbolId),    // Named type (record, choice, alias)
    Fn { params, ret }, // Function type
    Tuple(TyReprs), // Tuple type
    Array(TyRepr, len), // Array type
    Ptr(TyRepr),        // Pointer type
    Any,                // Gradual typing escape hatch
    Error,              // Type error placeholder
    // ...
}
```

VarId indexes into union-find structure in `Unifier`.

## Design Decisions

### 1. Phase 1: Visitor Pattern ✓

Use `ast::visitor.rs` for Phase 1 traversal. Can extend visitor if needed.

### 2. Reference Model: Hoisted Names ✓

**Unified approach** (like JS/TypeScript):

1. **Pass 1a**: Collect ALL top-level names (types, functions, values)
2. **Pass 1b**: Resolve ALL bodies with full visibility

```musi
// All names visible to each other (order doesn't matter for references)
val foo := fn () => bar();  // OK: bar visible via hoisting
val bar := fn () => foo();  // OK: foo visible

record A { b: B };           // OK: B visible
record B { a: A };           // OK: A visible
```

**Execution** is still top-to-bottom (last statement is entry point), but **name resolution** sees everything.

This gives you JS-style ergonomics with static typing.

### 3. Parallelization

Each function body is **independent** for type inference:

- Phase 1 produces complete signature types
- Phase 2 can infer each body separately
- No cross-function type variable sharing

**rayon v1.11.0 added** - can use `par_iter()` when ready.

### 4. Constraint-Based vs Immediate Unification

| Aspect | Immediate (Current) | Constraint-Based |
|--------|---------------------|------------------|
| When unify | During traversal | After collecting all |
| Error location | First conflict | Can show both sites |
| Implementation | Simpler | More complex |
| Error recovery | Stop at first | Can continue |
**Decision**: Immediate unification. Constraint-based may be considered if error messages need improvement.

### 5. Generics: Full Parametric Polymorphism ✓

**Chosen approach**: Parametric polymorphism with let-polymorphism (HM-style)

```musi
val identity := fn [T](x: T): T => x;  // ∀T. T → T

val a := identity(42);      // T inferred as Int
val b := identity("hello"); // T inferred as String
```

**Implementation**:

1. **Phase 1**: Store type params in `FnSig`: `[T, U, ...]`
2. **Phase 2**: At call site:
   - Create fresh type vars for each type param
   - Substitute type params with fresh vars in signature
   - Unify args with substituted param types
   - Result type uses unified vars

**Let-polymorphism** (generalization):

- After binding a `val`, generalize unbound type vars
- Each use instantiates with fresh vars
- Prevents value restriction issues

**Type representation**:

```rust
TyReprKind::Poly {
    params: Vec<TypeParamId>,  // [T, U]
    body: Box<TyRepr>,         // T → U
}
```
