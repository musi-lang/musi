# Semantic Analysis Refactoring Plan v2

## Type System Decisions

| Aspect | Decision |
|--------|----------|
| Type lattice | `∀T. Never <: T <: Any` |
| Unresolved vars | Default to `Any` (configurable via `mspackage.json`) |
| Subtyping | Coercive (`box`/`unbox`/`asinst` at runtime) |
| Polymorphism | Explicit `fn [T](...)` only |
| Operations on `Any` | Yield `Any`; use `as` to narrow |

## Architecture: Two Phases

```text
musi_sema/
├── lib.rs                    # Public API: bind()
├── types.rs                  # Re-exports
│
├── phase1/                   # Collection + Type Resolution
│   ├── mod.rs
│   ├── collector.rs          # Collect type/fn/val names (hoisting)
│   ├── resolver.rs           # Resolve type expressions, complete sigs
│   └── ctx.rs                # ResolveCtx (future use)
│
├── phase2/                   # Local Inference (per-function)
│   ├── mod.rs
│   ├── ctx.rs                # BindCtx
│   ├── expr.rs               # Expression binding
│   ├── pat.rs                # Pattern binding
│   ├── stmt.rs               # Statement binding
│   ├── ty.rs                 # Type expression resolution
│   └── ops.rs                # Coercions & type helpers
│
├── symbol.rs                 # Symbol table
├── unifier.rs                # Type unification
├── ty_repr.rs                # Type representation
├── semantic.rs               # SemanticModel output
├── builtins.rs               # Builtin types
└── error.rs                  # Errors
```

### Phase 1: Collection + Type Resolution

**Input**: AST
**Output**: Symbol table with names and resolved type signatures

- Collect all top-level names (types, functions, values)
- Resolve type expressions into TyRepr
- Complete function/record/choice/alias types
- Handle forward references via hoisting

### Phase 2: Local Inference

**Input**: AST + Phase 1 symbols/types
**Output**: Fully typed `SemanticModel`

- For each function body:
  1. Create local scope
  2. Bind parameters
  3. Infer expression types
  4. Unify return type with body
  5. Finalize unresolved vars → `Any`

## Migration Steps

1. ~~**Fix immediate error**: Remove parallel iteration~~ ✓
2. ~~**Consolidate phases**: 2-phase architecture~~ ✓
3. ~~**Extract ops.rs**: Type coercions & helpers~~ ✓
4. ~~**DRY expr.rs**: Extract shared helpers~~ ✓
5. **Add tests**: Unit tests for each phase

## Gradual Typing Rules

```text
Synthesis (↑):
  42 ↑ Int
  "hi" ↑ String
  (a, b) ↑ (A, B) where a ↑ A, b ↑ B

Checking (↓):
  e ↓ Any  always succeeds (box if needed)
  e ↓ T    where e ↑ S and S <: T

Subtyping (<:):
  Never <: T <: Any  for all T
  T <: T             reflexivity
  S → T <: S' → T'   if S' <: S and T <: T'  (contravariant params)
```

## Error Recovery

When inference fails:

1. Emit diagnostic
2. Return `TyRepr::error()`
3. `Error` type is compatible with everything (avoids cascading errors)
4. During finalization, unresolved vars → `Any`

## Not In Scope

- `mspackage.json` configuration format
- Codegen changes
- LSP integration changes
