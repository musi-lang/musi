# Semantic Analysis Refactoring Plan v2

## Problem Summary

The current `musi_sema` crate has grown complex:

- **Compile error**: Mutable borrow of `Interner` in parallel closure (`binder.rs:102`)
- **`phase2/expr.rs`**: 894 lines (violates 50-line per-function limit)
- **`BindCtx`**: God object with 9 mutable references
- **Parallel deferred tasks**: Complex fork/merge fighting with Rust's borrow checker

## Type System Decisions

| Aspect | Decision |
|--------|----------|
| Type lattice | `∀T. Never <: T <: Any` |
| Unresolved vars | Default to `Any` (configurable via `mspackage.json`) |
| Subtyping | Coercive (`box`/`unbox`/`asinst` at runtime) |
| Polymorphism | Explicit `fn [T](...)` only |
| Operations on `Any` | Yield `Any`; use `as` to narrow |

## Architecture: Three Phases

```text
musi_sema/
├── lib.rs                    # Public API: bind()
├── types.rs                  # Re-exports
│
├── phase1/                   # Collection (hoisted names)
│   ├── mod.rs
│   ├── collector.rs          # Collect type/fn/val names
│   └── resolver.rs           # Resolve type references
│
├── phase2/                   # Type Resolution
│   ├── mod.rs
│   ├── sigs.rs               # Complete function signatures
│   └── typedef.rs            # Complete record/choice/alias types
│
├── phase3/                   # Local Inference (per-function)
│   ├── mod.rs
│   ├── ctx.rs                # InferCtx (minimal state)
│   ├── expr.rs               # Expression inference
│   ├── pat.rs                # Pattern binding
│   ├── stmt.rs               # Statement binding
│   └── ops.rs                # Binary/unary operator typing
│
├── symbol.rs                 # Symbol table
├── unifier.rs                # Type unification
├── ty_repr.rs                # Type representation
├── semantic.rs               # SemanticModel output
├── builtins.rs               # Builtin types
└── error.rs                  # Errors
```

### Phase 1: Collection

**Input**: AST
**Output**: Symbol table with hoisted names (types unknown yet)

- Collect all top-level names (types, functions, values)
- No type resolution yet
- Just names and spans for error reporting

### Phase 2: Type Resolution

**Input**: AST + Phase 1 symbols
**Output**: Complete type information for declarations

- Resolve type expressions to `TyRepr`
- Complete function signatures (param types, return types)
- Complete record/choice/alias definitions
- Handle forward references (hoisted names)

### Phase 3: Local Inference

**Input**: AST + Phase 2 symbols/types
**Output**: Fully typed `SemanticModel`

- For each function body:
  1. Create local scope
  2. Bind parameters
  3. Infer expression types (bidirectional + HM)
  4. Unify return type with body
  5. Finalize unresolved vars → `Any`
- **Sequential** (no parallelism for now)

## Key Refactoring

### 1. Remove Parallelism

The parallel deferred task model is causing the compile error. Remove it:

```rust
// Before (complex fork/merge)
let results: Vec<_> = tasks.into_par_iter().map(|task| { ... }).collect();

// After (simple sequential)
for task in tasks {
    bind_function_body(&mut ctx, task);
}
```

### 2. Simplify Context

Split `BindCtx` into focused structs:

```rust
// Phase 2: Resolution context (read-heavy)
struct ResolveCtx<'a> {
    arena: &'a AstArena,
    symbols: &'a SymbolTable,
    interner: &'a Interner,  // Read-only
}

// Phase 3: Inference context (write-heavy)
struct InferCtx<'a> {
    arena: &'a AstArena,
    symbols: &'a mut SymbolTable,
    model: &'a mut SemanticModel,
    unifier: &'a mut Unifier,
    diags: &'a mut DiagnosticBag,
    in_loop: bool,
    in_fn: bool,
}
```

### 3. Break Up expr.rs

Current: 894 lines with 42 functions.

Split by concern:

- `expr.rs`: Main dispatcher, literals, identifiers, blocks, Function calls, pipe operator, if/while/for/match/return/break
- `ops.rs`: Binary/unary operators, type coercions

Try using parameterised helpers where possible.

### 4. DRY: Extract Type Helpers

Common patterns to extract:

- `ensure_numeric(ty, span)` → check/error
- `ensure_bool(ty, span)` → check/error
- `ensure_assignable(from, to, span)` → unify/error
- `fresh_var_or(annotation)` → annotation.unwrap_or(fresh_var())

## Migration Steps

1. **Fix immediate error**: Remove parallel iteration, use sequential loop
2. **Split phases**: Rename current phase2 → phase3, create new phase2
3. **Split expr.rs**: Extract `ops.rs`, `call.rs`, `control.rs`
4. **Simplify BindCtx**: Split into `ResolveCtx` + `InferCtx`
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

- Parallelism (can add later if profiling shows need)
- `mspackage.json` configuration format
- Codegen changes
- LSP integration changes
