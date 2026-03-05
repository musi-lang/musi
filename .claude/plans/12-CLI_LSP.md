# Phase 12 — CLI + LSP

**Crate:** `musi`, `musi_lsp`
**Goal:** Polish CLI subcommands, wire LSP for VS Code diagnostics and navigation.
**Dependencies:** Phase 7+ (grows with each subsequent phase)

---

## Deliverables

### CLI Subcommands

**`musi run <file.ms>`** — compile and execute (exists from Phase 5, polished here)
- Full pipeline: lex → parse → sema → codegen → VM exec
- Exit 0 on success, exit 1 on compile error, exit 2 on runtime error

**`musi check <file.ms>`** — type-check without executing
- Pipeline: lex → parse → sema
- Report all diagnostics
- Exit 0 if no errors, exit 1 if errors

**`musi build <file.ms> -o <out.mso>`** — compile to .mso without executing
- Pipeline: lex → parse → sema → codegen → serialize
- Write .mso to output path

**`musi dump-tokens <file.ms>`** — print token stream
- One token per line: `<span> <kind> [<text>]`
- Useful for debugging lexer

**`musi dump-ast <file.ms>`** — print AST as S-expressions
- Uses the S-expression dumper from Phase 3
- Useful for debugging parser

**Common flags:**
- `--no-color` — disable ANSI colors
- `--quiet` — suppress warnings, only show errors
- `-v` / `--verbose` — show timing info, phase progression

### Diagnostic Rendering

**Format:**
```
error: expected `)`, found `}`
  --> examples/test.ms:3:15
   |
 3 |   const x := (1 + 2};
   |               ^^^^^
   |                    expected `)` to match this `(`
```

**Features:**
- `file:line:col` header
- Source line display with line numbers
- Underline carets (`^`) under the error span
- Secondary labels (e.g., "to match this `(`")
- ANSI colors: red for errors, yellow for warnings, blue for notes
- Multi-line spans
- Context lines (1 line above and below)

**Implementation:**
```
fn render_diagnostic(diag: &Diagnostic, source_db: &SourceDb) → String:
  let (line, col) = source_db.lookup(diag.primary.file_id, diag.primary.span.start)
  let file_name = source_db.name(diag.primary.file_id)

  // Header
  emit "{severity}: {message}"
  emit "  --> {file_name}:{line}:{col}"

  // Source context
  let source_line = source_db.get_line(diag.primary.file_id, line)
  emit " {line} | {source_line}"
  emit "       {underline}"

  // Secondary labels
  for label in diag.secondary:
    emit secondary span + message
```

### LSP Server

**Crate:** `musi_lsp` using `tower-lsp`.

**Transport:** stdio (connects to VS Code extension).

**Capabilities (initial):**

1. **Diagnostics on change:**
   - On `textDocument/didOpen` and `textDocument/didChange`:
     - Re-lex + re-parse + re-check the file
     - Convert `Diagnostic` → LSP `Diagnostic`
     - Publish via `textDocument/publishDiagnostics`

2. **Go-to-definition:**
   - On `textDocument/definition`:
     - Find the identifier at cursor position
     - Look up in name resolution table → `DefId` → span
     - Return `Location` pointing to definition site
   - Works for: variables, functions, types, fields, variants

3. **Hover (type info):**
   - On `textDocument/hover`:
     - Find the expression at cursor position
     - Look up in type side table → `Type`
     - Format type as string
     - Return `Hover { contents: type_string }`

**Architecture:**
```
MusiLanguageServer = {
  source_db: SourceDb,
  documents: HashMap<Url, (String, Version)>,  // open documents
  analysis_cache: HashMap<Url, AnalysisResult>, // cached analysis
}

AnalysisResult = {
  tokens: Vec<Token>,
  ast: Program,
  diagnostics: Vec<Diagnostic>,
  name_table: HashMap<NodeId, DefId>,
  type_table: HashMap<NodeId, Type>,
}
```

**Incremental strategy (simple):**
- On each change, re-analyze the entire file from scratch.
- Cache the result. This is fast enough for single files.
- Future optimization: incremental parsing, dependency tracking.

### VS Code Extension Updates

Update `tools/vscode/` to:
- Register the LSP client (stdio transport, `musi-lsp` binary)
- Configure file associations (`.ms` → Musi language)
- Language configuration: brackets, comments, auto-closing pairs

**`extension.ts` (or `package.json` contribution):**
```json
{
  "contributes": {
    "languages": [{
      "id": "musi",
      "extensions": [".ms"],
      "configuration": "./language-configuration.json"
    }],
    "grammars": [{
      "language": "musi",
      "scopeName": "source.musi",
      "path": "./syntaxes/musi.tmLanguage.json"
    }]
  }
}
```

LSP client activation:
```
fn activate(context):
  server_path = find_musi_lsp_binary()
  client = new LanguageClient("musi", server_path, { stdio })
  client.start()
```

---

## Milestone

1. `musi check valid.ms` → exit 0, no output.
2. `musi check invalid.ms` → formatted diagnostics on stderr, exit 1.
3. `musi dump-tokens hello.ms` → correct token stream.
4. `musi dump-ast hello.ms` → correct S-expression.
5. `musi build hello.ms -o hello.mso` → valid .mso file.
6. Open `.ms` file in VS Code → squiggly underlines on errors.
7. Go-to-definition on a variable → jumps to its binding.
8. Hover over expression → shows inferred type.
9. `cargo test --workspace` passes.
