# Semantic LSP Support Plan

Add semantic token highlighting and full sema-phase integration to `musi_lsp`.

## Problem Summary

TextMate grammar provides syntax highlighting but cannot distinguish:

1. **Choice variant patterns** in non-definition contexts:

   ```musi
   match opt {
   case Some(v) => use(v),    // Some highlighted as entity.name.function
   case None => {}            // None highlighted as entity.name.type
   };

   if case Some(value) := opt { ... }; // Some highlighted as entity.name.function
   while case Some(item) := get_next() { ... }; // Some highlighted as entity.name.function
   ```

   These should be `variable.other.enummember` (like in choice definitions).

2. **Mutability distinction**:
   - `var` bindings should use underline (via LSP `mutable` modifier, same as rust-analyzer)
   - `val` bindings bound to literals become `variable.other.constant`

> [!NOTE]
> rust-analyzer uses the standard LSP `mutable` semantic token modifier. VS Code and most themes
> automatically underline tokens with this modifier - no custom theme contribution needed.

1. **Name-to-kind resolution**:
   - `val foo := fn ...` → `entity.name.function`
   - `val Point := record ...` `val Option := choice ...` → `entity.name.type`
   - `val bar := 5 + 5` → `variable.other.constant`

Q: Does binding start with `fn` after `:=`?
A: entity.name.function (semantic: case-insensitive)

Q: Does binding start with `record` or `choice` after `:=`?
A: entity.name.type (semantic: case-insensitive)

Q: Does binding `val` before identifier?
A: variable.other.constant (semantic: case-insensitive)

---

## Current LSP State

| Feature | Status |
|---------|--------|
| Document sync | ✅ Full |
| Document symbols | ✅ |
| Folding ranges | ✅ |
| Diagnostics | ✅ Lex + Parse errors only |
| Semantic analysis | ❌ Not integrated |
| Semantic tokens | ❌ Not implemented |
| Go to definition | ❌ |
| Hover (type info) | ❌ |

---

## Proposed Changes

### musi_sema

#### [MODIFY] [binder.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_sema/src/binder.rs)

Change `bind()` signature to also return `SymbolTable` (currently consumed internally by `Binder`):

```rust
pub fn bind(...) -> (SemanticModel, SymbolTable, DiagnosticBag)
```

> [!NOTE]
> **CLI compatibility:** `musi_cli` calls `binder::bind()` but discards the `SemanticModel`.
> Changing to 3-tuple return is safe - CLI updates to `(_, _, sema_bag)`.

---

### musi_lsp

#### [MODIFY] [state.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/state.rs)

Extend `GlobalState.parsed` to include semantic data:

```rust
pub struct AnalyzedDocument {
    pub prog: Prog,
    pub arena: AstArena,
    pub sema_model: SemanticModel,
    pub symbols: SymbolTable,
}
```

#### [MODIFY] [handlers.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/handlers.rs)

Call `musi_sema::bind()` in `analyze_and_publish` and store results. Publish sema diagnostics alongside parse diagnostics.

#### [NEW] [semantic_tokens.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/semantic_tokens.rs)

Implement `textDocument/semanticTokens/full` handler:

1. Walk AST collecting spans with their resolved `SymbolKind` + mutability
2. Map to LSP semantic token types:

| SymbolKind | TyRepr | Token Type | Modifiers |
|------------|--------|------------|-----------|
| `Local` | any | `variable` | `readonly` if `val` |
| `Local` | any | `variable` | `declaration` + (none) if `var` |
| `Param` | any | `parameter` | `readonly` (implicit val) |
| `Fn` | `Fn(...)` | `function` | `definition` at decl site |
| `Type` | any | `type` | |
| `Field` | any | `property` | `readonly` if `val` |
| `Variant` | any | `enumMember` | |
| `Builtin` | any | `function` | `defaultLibrary` |

1. Detect `val` bound to literals → `variable` + `readonly` modifier

#### [MODIFY] [server.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/server.rs)

Register `SemanticTokensProvider` capability with legend (token types + modifiers).

#### [MODIFY] [dispatch.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/dispatch.rs)

Add route for `textDocument/semanticTokens/full` request.

---

### tools/vscode

#### [MODIFY] [client.ts](file:///Users/krystian/CodeProjects/musi/tools/vscode/client/src/client.ts)

Add `semanticTokensProvider` to document selector if needed (vscode-languageclient handles most of this automatically).

---

## Phase 2 (Future)

Dependent on this work but out of scope for initial implementation:

- `textDocument/definition` (go to definition)
- `textDocument/hover` (show type on hover)
- `textDocument/references`
- `textDocument/rename`

---

## Verification Plan

### Automated Tests

```sh
# Ensure all existing tests pass
cargo test -p musi_sema

# Ensure LSP compiles
cargo build -p musi_lsp

# Run all workspace tests
cargo test
```

### Manual Verification

1. Build and install extension:

   ```sh
   cd tools/vscode && bun run package
   ```

2. Open VS Code with a `.ms` file containing:

   ```musi
   val Option := choice Option[T] { case Some(T), case None };

   val opt := Option.Some(42);

   match opt {
   case Some(v) => v,
   case None => 0
   };

   if case Some(value) := opt {
     value
   };

   val bar := 5;
   var counter := 0;
   ```

3. Verify:
   - `Some` and `None` in match/if-case are highlighted as enum members (not types)
   - `bar` appears as constant-style variable
   - `counter` has mutable indicator (typically underline depending on theme)

> [!IMPORTANT]
> The user should verify semantic highlighting visually in VS Code after installation.
