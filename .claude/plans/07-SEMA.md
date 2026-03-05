# Phase 7 — Name Resolution + Type System

**Crate:** `musi_sema`
**Goal:** Scope analysis + bidirectional type checking with HM-style inference.
**Dependencies:** Phase 3 (parser+AST)

---

## Deliverables

### Name Resolution

**Scope tree:**
```
Scope = {
  parent: Option<ScopeId>,
  bindings: HashMap<Symbol, DefId>,
}

DefId = u32 (newtype)  // unique per binding across the program

DefInfo = {
  id: DefId,
  name: Symbol,
  kind: DefKind,        // Const | Var | Fn | Type | Param | Variant
  span: Span,
  ty: Option<Type>,     // filled by type checker
}
```

**Algorithm (two-pass on each scope):**
1. **Collect declarations:** Walk the scope, register all `fn`, `record`, `choice`, `const`, `var`, `type` names. This allows forward references within a scope.
2. **Resolve references:** Walk expressions, look up each identifier in scope chain (inner → outer). Record `NodeId → DefId` mapping.

**Diagnostics:**
- Undefined name → error with suggestion (edit-distance based)
- Duplicate definition in same scope → error pointing at both
- Unused binding → warning

### Type Representation

```
Type =
  | Int | Float | Bool | String | Char | Unit
  | Tuple(Vec<Type>)
  | Array(Box<Type>, Option<usize>)
  | Arrow(Vec<Type>, Box<Type>)          // fn(A, B) -> C
  | Named(DefId, Vec<Type>)             // user-defined type + type args
  | Var(TypeVarId)                       // unification variable
  | Error                                // poison type (suppresses cascading)

TypeVarId = u32

TypeVar = {
  id: TypeVarId,
  binding: Option<Type>,                 // None = unresolved, Some = resolved
}
```

### Bidirectional Type Checker

Two modes:
- **Infer** (`infer(expr) → Type`): determine the type of an expression.
- **Check** (`check(expr, expected: Type)`): verify an expression against an expected type.

**Unification (union-find):**
```
fn unify(a: Type, b: Type) → Result<(), TypeError>:
  a = resolve(a)  // follow Var chains
  b = resolve(b)
  match (a, b):
    (Var(v), t) | (t, Var(v)) → bind v to t (occurs check)
    (Int, Int) → ok
    (Arrow(p1, r1), Arrow(p2, r2)) → unify pairwise
    (Named(d1, args1), Named(d2, args2)) →
      if d1 != d2: error
      unify args pairwise
    (Tuple(a), Tuple(b)) → unify pairwise, lengths must match
    _ → type mismatch error
```

**Type inference rules (selected):**

```
Γ ⊢ n : Int                              (int literal)
Γ ⊢ s : String                           (string literal)
Γ ⊢ true : Bool, false : Bool            (bool literal)

Γ ⊢ x : Γ(x)                            (variable lookup)

Γ ⊢ e₁ : A, Γ ⊢ e₂ : A                 (arithmetic, where A ∈ {Int, Float})
─────────────────────────────
Γ ⊢ e₁ + e₂ : A

Γ ⊢ f : Arrow([A₁,...,Aₙ], R), Γ ⊢ eᵢ : Aᵢ
──────────────────────────────────────────────
Γ ⊢ f(e₁,...,eₙ) : R

Γ ⊢ cond : Bool, Γ ⊢ then : T, Γ ⊢ else : T
──────────────────────────────────────────────
Γ ⊢ if cond then else : T

Γ ⊢ scrutinee : S, for each arm: Γ + pat_bindings(pᵢ, S) ⊢ eᵢ : T
───────────────────────────────────────────────────────────────────
Γ ⊢ match scrutinee { case pᵢ => eᵢ } : T
```

**Generics:**
- `fn id[T](x: T): T = x;` — `T` is a type parameter → fresh `TypeVar` at each call site.
- Instantiation: when calling a generic function, create fresh type vars for each type param, substitute into parameter/return types, then unify with actual arguments.

### Type Checking Specific Constructs

**Bindings:**
```
const x: T := e;  →  check(e, T)
const x := e;     →  infer(e) → T, record x : T
var x: T := e;    →  check(e, T)
```

**Assignments:**
```
x <- e;  →  look up x : T, must be var (not const), check(e, T)
```

**Records:**
```
record Point { x: Int, y: Int }
→ defines Named type Point with fields {x: Int, y: Int}
→ record literal .{ x := 1, y := 2 } checked field-by-field
```

**Choices:**
```
choice Option[T] { Some(T) | None }
→ defines Named type Option with type param T
→ Some(v) : check v against T
→ match arm case Some(x) => ... : bind x : T
```

### Output

Side table `NodeId → Type`. Codegen reads AST + side table. AST is not modified.

`Error` type: any expression with type `Error` suppresses further diagnostics involving that expression (prevents cascading errors).

---

## Milestone

1. `const x: Int := "hello";` → diagnostic: expected Int, found String, with correct span.
2. `const y := 1 + 2;` → infers `y : Int`.
3. Undefined variable → error with suggestion.
4. A well-typed program with records, choices, and generics passes cleanly.
5. `cargo test -p musi_sema` passes.
