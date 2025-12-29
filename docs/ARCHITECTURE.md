# Musi Compiler Architecture

> [!NOTE]
> This document describes the target architecture for the Musi compiler ecosystem.
> Current implementation may differ — see [Migration Roadmap](#migration-roadmap) for the path forward.

## Overview

Musi is a hand-written compiler with a layered crate architecture inspired by Roslyn (.NET) and rustc.

**Pipeline**: `Source → Lex → Parse → Sema → Codegen → VM`

**Key design principles**:

- **Separation of concerns**: Types are AST-agnostic, symbols are separate from binding
- **Unified diagnostics**: All errors flow through `musi_errors`
- **Driver orchestration**: CLI/LSP/REPL share `musi_driver` for consistency
- **GC opt-out**: Via `@[no_gc]` attribute, not lexical scopes

---

## Crate Dependency Graph

```text
                                 BINARIES
                    ┌──────────────┬──────────────┐
                    │              │              │
                musi_cli        musi_lsp         musi
                    │              │              │
                    └──────────────┴──────────────┘
                                   │
                                   ▼
                             musi_driver ◀────────────┐
                    ┌──────────────┼──────────────┐   │
                    │              │              │   │
                    ▼              ▼              ▼   │
              musi_resolve   musi_lint      musi_codegen
                    │              │              │
                    │              ▼              ▼
                    │         musi_sema ◀── musi_mono
                    │              │
                    ▼              ▼
               musi_ast       musi_symbols
                    │              │
                    ▼              ▼
               musi_parse     musi_types
                    │              │
                    ▼              │
               musi_lex            │
                    │              │
                    └──────┬───────┘
                           ▼
                      musi_errors
                           │
                           ▼
                      musi_basic

                        ───────

                       musi_rt
                           │
                    ┌──────┴──────┐
                    ▼             ▼
                musi_vm      musi_jit (future)

                        ───────

                      musi_fmt ──▶ musi_ast, musi_lex
```

---

## Crate Reference

### Foundation Layer

| Crate | Purpose |
|-------|---------|
| `musi_basic` | Core primitives: `Span`, `FileId`, `NodeId`, `InternedString`, arena types |
| `musi_errors` | Unified diagnostics: `Diagnostic`, `ErrorCode`, `Severity`, rendering |

### Type System Layer

| Crate | Purpose |
|-------|---------|
| `musi_types` | Type algebra, constructors (fn, tuple, ptr, array, optional), unification engine, HM inference. **AST-agnostic** — no dependency on syntax. |

### Syntax Layer

| Crate | Purpose |
|-------|---------|
| `musi_lex` | Lexer: source → tokens |
| `musi_ast` | AST node definitions with typed arena allocation (`NodeId<T>`) |
| `musi_parse` | Parser: recursive descent + Pratt for operator precedence |

### Semantic Layer

| Crate | Purpose |
|-------|---------|
| `musi_symbols` | Symbol definitions (`FnSymbol`, `TypeSymbol`, `VarSymbol`), symbol tables, scopes, builtins/prelude |
| `musi_resolve` | Module resolution: import maps, `paths` aliases, `baseUrl`, physical file lookup |
| `musi_sema` | Core semantic analysis: declaration phase (collect symbols), binding phase (resolve names, type-check) |
| `musi_lint` | Configurable lint rules with `@[allow/deny/warn(...)]` attributes |
| `musi_mono` | Monomorphization: generic instantiation (e.g., `List[Int]` → `List_Int`) |

### Backend Layer

| Crate | Purpose |
|-------|---------|
| `musi_codegen` | Bytecode emission: `.mso` file format, constant pool, symbol table generation |

### Runtime Layer

| Crate | Purpose |
|-------|---------|
| `musi_rt` | Runtime core: garbage collector, FFI bridge, memory allocator, intrinsics |
| `musi_vm` | Bytecode interpreter: dispatch loop, operand stack, call frames |
| `musi_jit` | *(future)* JIT compiler for hot paths |

### Tooling Layer

| Crate | Purpose |
|-------|---------|
| `musi_fmt` | Code formatter (configurable via `mspackage.json` `fmt.*` options) |
| `musi_driver` | Orchestration: package config parsing, module graph, incremental compilation, watch mode |

### Binary Layer

| Crate | Purpose |
|-------|---------|
| `musi_cli` | CLI binary: `musi build`, `musi run`, `musi fmt`, `musi lint`, etc. |
| `musi_lsp` | Language Server Protocol implementation |
| `musi` | Interactive REPL |

---

## Compilation Phases

```text
┌─────────────────────────────────────────────────────────────────────────────┐
│                           COMPILATION PHASES                                │
└─────────────────────────────────────────────────────────────────────────────┘

1. CONFIG PHASE (musi_driver)
   mspackage.json → CompilerOptions, ModuleGraph, LintConfig, FmtConfig

2. RESOLVE PHASE (musi_resolve)
   import "foo" → physical file path
   Uses: paths aliases, imports map, baseUrl

3. LEX PHASE (musi_lex)
   Source → Tokens

4. PARSE PHASE (musi_parse)
   Tokens → SyntaxTree (AST)

5. DECLARATION PHASE (musi_sema/phase1)
   AST → Symbols collected into SymbolTable
   "What names exist in this scope?"

6. BIND PHASE (musi_sema/phase2)
   Names → Symbols resolved
   Expressions → Type-checked
   "What do names refer to? Do types match?"

7. LINT PHASE (musi_lint)
   Typed AST → Diagnostics
   Configurable rules from mspackage.json + @[allow/deny/warn]

8. MONO PHASE (musi_mono)
   Generic instantiation
   List[Int] → concrete List_Int implementation

9. CODEGEN PHASE (musi_codegen)
   Typed AST → .mso bytecode

10. EMIT PHASE (musi_driver)
    Write .mso files to outDir
    Generate .msbuildinfo for incremental builds
```

---

## Lint System

Rust-style attribute-driven linting:

**In code:**

```musi
@[allow("unused_vars")]
fn foo(x: Int32) { ... };

@[deny("implicit_any")]
record Bar { ... };

@[warn("missing_docs")]
export fn public_api() { ... };
```

**In `mspackage.json`:**

```json
{
  "lint": {
    "rules": {
      "deny": ["implicit_any", "missing_cases_in_match"],
      "warn": ["unused_vars", "unused_parameters"],
      "allow": ["unreachable_code"]
    }
  }
}
```

**Precedence**: Attribute > Package config > Default

---

## Error Handling

All diagnostics flow through `musi_errors`:

```text
musi_errors
├── Diagnostic          // Error/Warning/Hint with span, message, code
├── DiagnosticBuilder   // Fluent API for constructing errors
├── DiagnosticSink      // Collects diagnostics (deferred emission)
├── ErrorCode           // E0001, W0042, etc.
├── Severity            // Error | Warning | Hint | Note
└── Renderer            // Pretty-print for CLI (colors, underlines, suggestions)
```

Example usage:

```rust
sink.error(ErrorCode::E0001)
    .with_message("type mismatch")
    .with_span(expr.span)
    .with_label(expected_span, "expected `Int32`")
    .with_label(actual_span, "found `String`")
    .emit();
```

---

## Runtime Architecture

```text
┌─────────────────────────────────────────────────────────────────┐
│                          musi_rt                                │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐             │
│  │     GC      │  │     FFI     │  │   Memory    │             │
│  │  Collector  │  │   Bridge    │  │  Allocator  │             │
│  └─────────────┘  └─────────────┘  └─────────────┘             │
│  ┌─────────────┐  ┌─────────────┐                              │
│  │ Intrinsics  │  │  Builtins   │                              │
│  │ (@[no_gc])  │  │  (String)   │                              │
│  └─────────────┘  └─────────────┘                              │
└─────────────────────────────────────────────────────────────────┘
                              │
               ┌──────────────┴──────────────┐
               ▼                             ▼
┌─────────────────────────┐    ┌─────────────────────────┐
│        musi_vm          │    │     musi_jit (future)   │
│  ┌───────────────────┐  │    │  ┌───────────────────┐  │
│  │  Dispatch Loop    │  │    │  │   IR Generation   │  │
│  │  Operand Stack    │  │    │  │   Native Codegen  │  │
│  │  Call Frames      │  │    │  │   Code Cache      │  │
│  └───────────────────┘  │    │  └───────────────────┘  │
└─────────────────────────┘    └─────────────────────────┘
```

**GC opt-out** is controlled via `@[no_gc]` attribute on functions or types, not lexical `unsafe` blocks.

---

## Migration Roadmap

### Current State

```text
musi_basic → musi_lex → musi_ast → musi_parse → musi_sema → musi_codegen
                                  ↓
                               musi_lsp
                               musi_cli
                               musi (REPL)
```

### Migration Steps

| Priority | Action | Unblocks |
|----------|--------|----------|
| 1 | Create `musi_errors` | Unified diagnostics everywhere |
| 2 | Extract `musi_types` from `musi_sema` | AST-agnostic type system |
| 3 | Extract `musi_symbols` from `musi_sema` | Clean symbol table API |
| 4 | Create `musi_resolve` | Proper module resolution |
| 5 | Create `musi_driver` | Unified CLI/LSP/REPL orchestration |
| 6 | Create `musi_lint` | Configurable linting |
| 7 | Create `musi_rt` | Runtime foundation |
| 8 | Create `musi_vm` | Bytecode execution |
| 9 | Implement `musi_codegen` | End-to-end compilation |
| 10 | Create `musi_fmt` | Code formatting |

---

## Package Configuration

All configuration lives in `mspackage.json` (see [schema](../schemas/mspackage-schema.v1.json)):

- **Package metadata**: `name`, `version`, `dependencies`, `exports`
- **Compiler options**: `compilerOptions.target`, `strict`, `noImplicitAny`, etc.
- **Module resolution**: `paths`, `imports`, `baseUrl`
- **Tooling**: `fmt.*`, `lint.*`, `test.*`, `bench.*`
- **Build**: `include`, `exclude`, `references`, `incremental`

---

## Related Documentation

- [BYTECODE.md](./BYTECODE.md) — Bytecode format specification
- [RUNTIME.md](./RUNTIME.md) — Runtime system details
- [grammar.ebnf](../grammar.ebnf) — Language grammar (source of truth)
