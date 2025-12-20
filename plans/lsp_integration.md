# LSP Integration Plan

This document outlines the architecture for the Musi Language Server (MusiLSP) and its integration with VS Code.

## Architecture

The system consists of two main components:

1. **LSP Server (OCaml)**: The core language server running as a standalone binary (`musi-lsp`).
2. **VS Code Extension (JavaScript)**: A lightweight client that spawns the OCaml server and handles VS Code specific integration.

### 1. LSP Server (OCaml)

We will use the standard OCaml `lsp` library for protocol handling. The server will be packaged as a binary executable.

#### Library Stack

- **`lsp`**: For parsing and generating LSP JSON-RPC messages.
- **`linol` (Potential alternative)**: Provides a higher-level framework if `lsp` is too low-level, but direct `lsp` usage is often preferred for control. We will target `lsp`.
- **`yojson`**: For JSON serialization (dependency of `lsp`).
- **`cmdliner`**: For CLI argument parsing.

#### Phase 1: Syntax-Only Capabilities (Current Target)

We can achieve significant functionality using only the Parser and AST, without Semantic Analysis (`lib/sema`):

- **Diagnostics**:
  - Syntax errors from the parser (e.g., "expected ';'").
- **`textDocument/documentSymbol` (Outline)**:
  - Traverse AST to list functions, types, and top-level variables.
- **`textDocument/foldingRange`**:
  - Derived from AST node spans (blocks, imports, match cases).
- **`textDocument/selectionRange`**:
  - Smart selection expansion based on AST node hierarchy.
- **`textDocument/formatting`**:
  - (Optional) Basic formatting by printing the AST/CST.

#### Future Capabilities (Requires Sema)

- `textDocument/hover`: Type information.
- `textDocument/definition`: Jump to definition (requires symbol resolution).
- `textDocument/completion`: Type-aware completion.

#### Directory Structure

```text
bin/
  lsp/
    dune      # Executable definition
    main.ml   # Entry point
lib/
  lsp/        # Reusable LSP logic (if separated)
```

### 2. VS Code Extension (JavaScript/Bun)

The extension will be built using the standard VS Code extension API and `vscode-languageclient`.

#### Runtime

- **Bun**: Used for package management and building/bundling the extension.
- **Node.js**: The extension runtime within VS Code is Node.js-compatible.

#### features

- **Server Management**:
  - Automatically find the `musi-lsp` binary (e.g., in `_build/default/bin/lsp/main.exe` or a configured path).
  - Spawn the server process.
  - Restart server command.
- **Syntax Highlighting**:
  - `musi.tmLanguage.json` (TextMate grammar) for basic highlighting even if the server is off.

#### Directory Structure (`tools/vscode/`)

```text
tools/vscode/
  package.json
  jsconfig.json
  src/
    extension.js   # Client activation
  syntaxes/
    musi.tmLanguage.json
```

## CLI Interface (`bin/main.exe`)

The main compiler binary will act as the entry point.

- `musi lsp`: Starts the language server over stdio.
- `musi check <file>`: distinct command for CLI-based checking.

## Development Workflow

1. **Build Server**: `dune build bin/lsp`
2. **Build Extension**: `cd tools/vscode && bun install && bun run package`
3. **Debug**: Use VS Code's "Extension Development Host" to launch the extension, which connects to the locally built OCaml binary.
