# Plan 3: LSP Server Integration

## Goal

Add semantic features to LSP server using `SemanticModel`.

## Phase A: SemanticModel Enhancements (`musi_sema`)

### [MODIFY] `model.rs`

Add LSP-relevant APIs:

```rust
pub fn expr_at_span(&self, arena: &AstArena, target: Span) -> OptExprId;
pub fn symbol_at_span(&self, target: Span) -> OptSymbolId;
pub fn refs_of(&self, symbol: SymbolId) -> &[Span];
```

Add new fields:

```rust
symbol_refs: HashMap<SymbolId, Spans>,
```

## Phase B: LSP Caching (`musi_lsp`)

### [MODIFY] `state.rs`

Add semantic model caching:

```rust
semantic_models: HashMap<lsp_types::Url, SemanticModel>,
file_versions: HashMap<lsp_types::Url, i32>,
```

### [MODIFY] `handlers.rs`

Use `SemanticModel` for:

- Hover: `type_of_expr` at cursor position
- Go to Definition: `symbol_of_expr` → `symbol.def_span`
- Find References: `refs_of(symbol)`

## Phase C: VSCode Client (`tools/vscode/client/src`)

May need updates for new capabilities (textDocument/hover, etc.).

## Verification

```sh
cargo build -p musi_lsp
```
