# Semantic LSP Support - Phase 2

Depends on Phase 1 (semantic tokens). Adds navigation and introspection features.

---

## Proposed Changes

### musi_sema

#### [NEW] [query.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_sema/src/query.rs)

Query API for LSP lookups:

```rust
pub fn symbol_at_position(
    model: &SemanticModel,
    arena: &AstArena,
    offset: usize,
) -> Option<SymbolId>;

pub fn references_to_symbol(
    model: &SemanticModel,
    arena: &AstArena,
    symbol: SymbolId,
) -> Spans;
```

---

### musi_lsp

#### [NEW] [definition.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/definition.rs)

Handle `textDocument/definition`:

1. Convert LSP position → byte offset
2. Call `symbol_at_position` to find symbol under cursor
3. Look up `Symbol.def_span` in `SymbolTable`
4. Convert span → LSP `Location`

#### [NEW] [hover.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/hover.rs)

Handle `textDocument/hover`:

1. Find symbol at position
2. Get `TyRepr` from `SemanticModel.type_of_expr` / `type_of_pat`
3. Format type using `Display` impl
4. Return `Hover { contents: MarkupContent { kind: Markdown, value: ... } }`

Example output:

```musi
val counter: Int32
```

#### [NEW] [references.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/references.rs)

Handle `textDocument/references`:

1. Find symbol at position
2. Collect all spans referencing that symbol
3. Convert to `Location[]`

#### [NEW] [rename.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/rename.rs)

Handle `textDocument/rename` and `textDocument/prepareRename`:

1. Verify symbol at position is renameable (not builtin)
2. Collect all reference spans + definition span
3. Build `WorkspaceEdit` with text edits

#### [MODIFY] [server.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/server.rs)

Register capabilities:

- `definitionProvider`
- `hoverProvider`
- `referencesProvider`
- `renameProvider` + `prepareRenameProvider`

#### [MODIFY] [dispatch.rs](file:///Users/krystian/CodeProjects/musi/crates/musi_lsp/src/dispatch.rs)

Add routes for new request types.

---

## Verification Plan

### Manual Verification

1. **Go to Definition**: Ctrl+Click on variable → jumps to declaration
2. **Hover**: Mouse over identifier → shows type
3. **Find References**: Right-click → Find All References
4. **Rename**: F2 on identifier → renames all occurrences

### Automated Tests

```sh
cargo test -p musi_sema
cargo test -p musi_lsp
```

---

## Phase 3 (Future)

- `textDocument/completion` (autocomplete)
- `textDocument/signatureHelp` (function param info)
- `textDocument/codeAction` (quick fixes)
- `workspace/symbol` (project-wide symbol search)
