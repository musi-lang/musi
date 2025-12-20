# Tooling Roadmap: `fmt`, `lint`, and `lsp`

This document outlines the plan for implementing the detailed logic behind the `fmt` and `lint` commands, and their integration with the Language Server (LSP).

## Overview

The `music` binary already supports the CLI entry points:

- `music fmt [files]`
- `music lint [files]`

The next goal is to implement the actual engines for these commands.

## Components

**Architecture**:

- **Location**: `tools/fmt` (Separate from core `lib/`).
- **Structure**:
  - `tools/fmt/lib`: The formatting logic (reusable).
  - `tools/fmt/bin`: Standalone `musi-fmt` (optional, if desired).
- **Engine**:
  - Build on `lib/parse` to generate a CST (Concrete Syntax Tree) or AST including comments/trivia.
  - *Note*: If the current AST drops comments, we may need a "Resilient Parser" or separate "CST" module.
  - Use a pretty-printer library (e.g., `Format` or specialized algebra) to emit formatted code.
- **Integration**:
  - **CLI**: `music fmt` links against `musi_tools.fmt` library.
  - **LSP**: `textDocument/formatting` request calls `musi_tools.fmt`.

**Future Work Items**:

- [ ] Design CST with trivia (whitespace/comments) preservation.
- [ ] Implement `tools/fmt` printer.
- [ ] Configuration support (minimal, e.g., via `msconfig.json` -> `fmt` section).

### 2. `musi-lint` (Linter)

**Goal**: Static analysis tool to catch common mistakes, style violations, and potential bugs.

**Architecture**:

- **Location**: `tools/lint` (Separate from core `lib/`).
- **Structure**:
  - `tools/lint/lib`: The linting logic.
- **Engine**:
  - Traverse the AST/CST.
  - Apply a set of rules (configurable).
- **Integration**:
  - **CLI**: `music lint` reports diagnostics to stderr (or JSON output).
  - **LSP**: Push `textDocument/publishDiagnostics`.

**Future Work Items**:

- [ ] Define initial rule set (e.g., `no-unused-vars` - already in sema?, `naming-convention`).
- [ ] Implement rule runner.

### 3. `musi-lsp` (Language Server)

**Goal**: Provide IDE features (VS Code, etc.) using `musi-fmt` and `musi-lint`.

**Architecture**:

- **Location**: `tools/lsp` (Separate from core `lib/`).
- **Structure**:
  - `tools/lsp/lib`: Reusable LSP logic (if separated).
  - `tools/lsp/bin`: Standalone `musi-lsp` binary.

- **Binary**: `bin/lsp/main.exe` (or integrated into `music lsp`).
- **Communication**: Stdio JSON-RPC.
- **State**:
  - Maintain an in-memory VFS (Virtual File System) of open documents.
  - Debounced analysis on change.

**Workflow**:

1. User opens file.
2. LSP initializes.
3. User types -> LSP updates VFS -> Runs `Parser` -> Reports Syntax Errors.
4. User saves (or triggers format) -> LSP runs `Musi_fmt` -> Returns edits.
5. Background -> LSP runs `Musi_lint` -> Pushes warnings.

## Reference CLI Usage

```bash
# Format specific files in place
$ music fmt src/main.ms

# Check format (CI mode)
$ music fmt --check src/

# Lint project
$ music lint src/
```
