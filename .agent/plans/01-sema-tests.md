# Plan 1: Sema Unit Tests

## Goal

Expand `musi_sema` tests to ensure correctness before LSP integration.

## Tests to Add

### Unifier (`unifier/tests.rs`) — 10 tests

- `unify_tuple_tys`, `unify_tuple_mismatch`
- `unify_array_tys`, `unify_array_size_mismatch`
- `unify_fn_tys`, `unify_fn_arity_mismatch`
- `unify_optional_ty`, `unify_ptr_ty`
- `unify_transitive_vars`, `finalize_unbound_var`

### Binder (`binder/tests.rs`) — 12 tests

- Functions: `bind_fn_basic`, `bind_fn_ret_ty`, `bind_fn_params`
- Operators: `bind_binary_arithmetic`, `bind_binary_relational`, `bind_unary_neg`
- Collections: `bind_array_lit`, `bind_tuple_lit`, `bind_index_expr`
- Control: `bind_match_basic`, `bind_for_iterator`
- Patterns: `bind_pat_tuple`

### SymbolTable (NEW: `symbol/tests.rs`) — 5 tests

- `scope_push_pop`, `define_lookup`, `shadowing_same_scope_error`
- `shadowing_nested_scope_ok`, `lookup_in_parent_scope`

## Verification

```sh
cargo test -p musi_sema
```
