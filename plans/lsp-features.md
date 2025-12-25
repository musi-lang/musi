# Musi LSP Implementation Plan (Rust)

## Overview

Implement a Rust-based LSP server for Musi language, supporting pre-semantic level features (lex → parse level). The VSCode extension client already exists in TypeScript and will be updated to connect to the Rust server.

---

## Libraries

### Server (Rust)

| Library | Purpose |
|---------|---------|
| [`async_lsp`](https://github.com/oxalica/async-lsp) | **Primary LSP framework**. Async, tower-based, actively maintained successor to tower-lsp. Fixes notification ordering issues. |
| `tokio` | Async runtime |
| `serde_json` | JSON serialization for LSP messages |

**Why not alternatives:**

- `tower-lsp`: Last updated 2+ years ago, has notification ordering bugs
- `lsp-server`: Synchronous (crossbeam channels), requires manual async management

**Recommendation**: `async_lsp` - modern, async, handles concurrent requests properly while keeping notifications in order.

### Client (TypeScript)

| Library | Purpose |
|---------|---------|
| `vscode-languageclient` | ✅ Already in use |
| `vscode` | ✅ Already in use |

**Answer to "can client be Rust?"**: No. VSCode extensions must be JavaScript/TypeScript. The client starts/communicates with the server but runs in the VS Code process.

---

## Project Structure

```
crates/
├── musi_lsp/              # NEW: LSP server crate
│   ├── Cargo.toml
│   └── src/
│       ├── main.rs        # Entry point, starts server
│       ├── server.rs      # LanguageServer trait impl
│       ├── document.rs    # Document management
│       ├── symbols.rs     # Document symbols
│       ├── diagnostics.rs # Parse error diagnostics
│       ├── completion.rs  # Keyword/identifier completion
│       └── format.rs      # Code formatting
│
tools/vscode/
├── client/src/extension.ts  # Update paths to Rust binary
└── ...
```

---

## Pre-Semantic Features (What musi_ast + musi_parse enable)

### ✅ Achievable Now (No musi_sema needed)

| Feature | LSP Method | Implementation |
|---------|------------|----------------|
| **Syntax Diagnostics** | `textDocument/publishDiagnostics` | Parse errors from `musi_parse` |
| **Document Symbols** | `textDocument/documentSymbol` | Extract from AST: fn, record, sum, val, var, alias |
| **Keyword Completion** | `textDocument/completion` | Static list of Musi keywords |
| **Identifier Completion** | `textDocument/completion` | Collect identifiers from parsed AST |
| **Code Formatting** | `textDocument/formatting` | AST-based pretty printer |
| **Bracket Matching** | Built into TextMate | ✅ Already works |
| **Folding Ranges** | `textDocument/foldingRange` | Block expressions, record/sum bodies |
| **Selection Ranges** | `textDocument/selectionRange` | AST-based expansion |

### ❌ Requires musi_sema (Future)

| Feature | Why it needs sema |
|---------|-------------------|
| Go to Definition | Need symbol resolution |
| Find References | Need reference tracking |
| Hover (type info) | Need type inference |
| Rename | Need scope analysis |
| Semantic Tokens | Need to distinguish variables vs types |

---

## Implementation Phases

### Phase 1: Server Skeleton

1. Create `crates/musi_lsp` crate
2. Set up `tower-lsp` with basic lifecycle handlers
3. Update VSCode extension to find Rust binary
4. Verify connection works

### Phase 2: Syntax Diagnostics

1. On `textDocument/didOpen` and `textDocument/didChange`:
   - Tokenize with `musi_lex`
   - Parse with `musi_parse`
   - Collect errors from `DiagnosticBag`
   - Push diagnostics to client

### Phase 3: Document Symbols

1. Walk parsed AST
2. Extract symbols: `fn`, `record`, `sum`, `val`, `var`, `alias`, `import`
3. Build hierarchy (nested functions, record fields, sum cases)
4. Return `DocumentSymbol[]`

### Phase 4: Completion

1. Keyword completion: static list from grammar
2. Identifier completion: collect all idents from current document AST
3. Type name completion: collect type idents

### Phase 5: Formatting

1. Implement AST pretty-printer
2. Handle all ExprKind/PatKind/TypKind variants
3. Configurable indent width

### Phase 6: Folding & Selection

1. Folding: blocks, record/sum bodies, match arms
2. Selection: expand from cursor position through AST hierarchy

---

## Cargo.toml for musi_lsp

```toml
[package]
name = "musi_lsp"
version = "0.1.0"
description = "Language server for Musi"
version.workspace = true
edition.workspace = true
repository.workspace = true
readme = "README.md"
license.workspace = true
keywords.workspace = true
categories.workspace = true

[[bin]]
name = "musi_lsp"
path = "src/main.rs"

[dependencies]
async_lsp = "0.2.2"
tokio = { version = "1.48.0", features = ["full"] }
serde_json = "1.0.147"

musi_basic = { path = "../musi_basic" }
musi_lex = { path = "../musi_lex" }
musi_parse = { path = "../musi_parse" }
musi_ast = { path = "../musi_ast" }
```

---

## VSCode Extension Updates

Update `findServerPath()` in `extension.ts`:

```typescript
const candidates = [
  path.join(workspacePath, "target", "debug", "musi_lsp"),
  path.join(workspacePath, "target", "release", "musi_lsp"),
  "/usr/local/bin/musi_lsp",
];
```

---

## Questions for Clarification

1. **Binary location preference**: Should the LSP binary be:
   - `target/debug/musi_lsp` (default cargo location)
   - `tools/bin/musi_lsp` (separate binary folder)
   - Installed globally?

2. **Formatting style**: Any specific preferences for:
   - Indent size (2 or 4 spaces)?
   - Max line length?
   - Brace style?

3. **Priority features**: Which to implement first after diagnostics?
   - Symbols (outline view)
   - Completion (productivity)
   - Formatting (consistency)

---

## Verification Plan

### Automated

- `cargo test -p musi-lsp`
- Integration tests connecting to server

### Manual

- Open `.ms` file in VSCode
- Verify error squiggles appear for syntax errors
- Test outline view (Cmd+Shift+O)
- Test completion trigger
