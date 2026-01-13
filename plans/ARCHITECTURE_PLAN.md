# Musi Compiler Architecture Refactoring Plan

## 1. Executive Summary

This document details the migration from the 14-crate prototype architecture to a 7-layer production architecture. The refactoring consolidates fragmented crates, eliminates circular dependencies, and establishes clear phase boundaries.

**Current State**: 10 active crates with significant duplication  
**Target State**: 7 layers with strict dependency ordering  
**Implementation Time**: 2-3 weeks  
**Risk Level**: Medium (mechanical changes, no algorithmic modifications)

## 2. Current Architecture Assessment

### 2.1 Active Crates Analysis

| Crate | Lines | Duplication | Dependencies | Analysis |
|-------|-------|-------------|--------------|----------|
| musi_core | 1274 | token.rs, diag.rs | - | Foundation overloaded with language concepts |
| musi_source | 103 | None (new) | core | Clean implementation |
| musi_token | 441 | token.rs exists in core | core | Duplicate token definitions |
| musi_diags | 8 | diag.rs exists in core | core | Single lib.rs file |
| musi_ast | 821 | None | core | Appropriate size |
| musi_lex | 512 | None | core, token | Appropriate size |
| musi_parse | 1900 | None | core, token, lex, ast | Appropriate size |
| musi_sema | 1884 | Defines Ty/TyKind | core, ast | Type system should be separate layer |
| musi_lsp | 600+ | Multiple deps | core, parse, sema, ast | Kitchen sink pattern |
| musi_cli | 150+ | Minimal logic | all | Appropriate as driver |

**Total**: 6,593 lines across 10 crates (avg 659 lines/crate)

### 2.2 Design Violations

1. **Token Duplication**: `musi_core/src/token.rs` and `musi_token/src/token.rs` contain identical definitions
2. **Diagnostic Fragmentation**: `musi_core/src/diag.rs` vs `musi_diags/src/lib.rs`
3. **Source Overlap**: `musi_core/src/source.rs` vs `musi_source/` crate
4. **Type System Co-location**: Type inference and semantic analysis share crate
5. **LSP Kitchen Sink**: Depends on parser, semantic analyzer, and AST simultaneously

### 2.3 Circular Dependency Risk

```
musi_sema dev-depends on musi_parse (for tests)
    ↓ potential cycle
musi_parse → musi_ast → musi_core ← musi_sema
```

The semantic analyzer requires parser for test fixtures, suggesting tight coupling.

## 3. Target Architecture Specification

### 3.1 Layer Structure

```
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│ musi_driver  │  │ musi_bytecode│  │ musi_types   │
│ (Layer 7)    │  │ (Layer 5)    │  │ (Layer 4)    │
└──────┬───────┘  └──────┬───────┘  └──────┬───────┘
       │                 │                 │
       ↓                 ↓                 ↓
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   musi_vm    │  │ musi_syntax  │  │ musi_source  │
│ (Layer 6)    │  │ (Layer 3)    │  │ (Layer 2)    │
└──────┬───────┘  └──────┬───────┘  └──────┬───────┘
       │                 │                 │
       └─────────────────┴─────────────────┘
                         ↓
               ┌──────────────┐
               │  musi_core   │
               │  (Layer 1)   │
               └──────────────┘
```

### 3.2 Crate Consolidation Matrix

| Current → Target | Rationale |
|-----------------|-----------|
| musi_core (partial) → musi_core | Strip token/source/diag from core |
| musi_token + musi_ast + musi_lex + musi_parse → musi_syntax | Lexer/parser/AST co-dependent |
| musi_sema (type parts) → musi_types | Types are standalone |
| musi_sema (semantic parts) → removed | Merge into types |
| musi_source (keep) → musi_source | Clean utility |
| musi_diags (partial) → musi_syntax | Diags belong with syntax |
| musi_cli + musi_lsp → musi_driver | Single entry point |
| (new) → musi_bytecode | Bytecode IL + emission |
| (new) → musi_vm | Runtime VM (standalone) |

### 3.3 Interface Boundaries

**Layer 3 (musi_syntax)** → exports:
```rust
pub use token::{Token, TokenKind};
pub use ast::{Expr, Stmt, Pat, AstArena, parse};
pub use lex::{Lexer, tokenize};
pub use diag::{Diagnostic, DiagnosticBag, emit};
```

**Layer 4 (musi_types)** → exports:
```rust
pub use ty::{Ty, TyKind, TyArena, TyId};
pub use infer::{Inferer, infer_expr};
pub use unifier::{Unifier, unify};
```

**Layer 5 (musi_bytecode)** → exports:
```rust
pub use bytecode::{BytecodeModule, Opcode, Instruction};
pub use emit::{EmitterContext, emit_program};
```

**Layer 6 (musi_vm)** → exports:
```rust
pub use vm::{Vm, Value, execute};
// C API in capi/
pub mod capi;
```

## 4. Refactoring Operations

### 4.1 Layer 1: purify musi_core

**Operation**: Remove language-specific concepts from foundation

**Files to remove**:
- `crates/musi_core/src/token.rs` (441 lines) → move to musi_syntax
- `crates/musi_core/src/source.rs` (156 lines) → move to musi_source
- `crates/musi_core/src/diag.rs` (228 lines) → move to musi_syntax

**Core retained**:
- `arena.rs`: Arena allocation
- `span.rs`: Source positions
- `interner.rs`: String interning
- `error.rs`: Base error type

**Result**: musi_core reduced to ~450 lines (pure foundation)

### 4.2 Layer 2: retain musi_source

**Operation**: Keep clean implementation, re-export from Layer 1

**Dependencies**: musi_core → musi_source

**Result**: 103 lines remain, provides SourceFile/SourceMap

### 4.3 Layer 3: merge token/ast/lex/parse

**Operation**: Consolidate syntax processing

**Merge sequence**:
```
1. Create musi_syntax crate
2. Move musi_token/ → musi_syntax/src/token/
3. Move musi_ast/src/* → musi_syntax/src/ast/
4. Move musi_lex/src/* → musi_syntax/src/lex/
5. Move musi_parse/src/* → musi_syntax/src/parse/
6. Move diagnostic code to musi_syntax/src/diag/
```

**Dependency resolution**:
- musi_syntax → musi_core, musi_source
- All internal modules: `pub(crate)` visibility where appropriate

**Expected size**: ~3500 lines (still under 4000 threshold)

### 4.4 Layer 4: extract type system

**Operation**: Create standalone type layer

**Extract from musi_sema**:
- `src/ty.rs` → `musi_types/src/ty.rs`
- `src/inferer.rs` → `musi_types/src/infer.rs`
- `src/table.rs` → `musi_types/src/unifier.rs`
- `src/ty_env.rs` → `musi_types/src/context.rs`

**Remove duplicate functionality**:
- Symbol resolution → stays in types (renamed to NameResolution)
- Semantic checks → merge with type checking

**Result**: musi_types (2000 lines), musi_syntax dependency

### 4.5 Layer 5: create bytecode layer

**Operation**: New crate for MSIL generation

**Components**:
```
musi_bytecode/
├── src/
│   ├── lib.rs
│   ├── module.rs      // BytecodeModule struct
│   ├── opcode.rs      // Opcode enum (0x00-0xFF)
│   ├── emit.rs        // EmitterContext and traversal
│   └── optimize.rs    // Peephole optimizations
└── tests/
    └── integration.rs // source → bytecode
```

**Dependencies**: musi_core, musi_types

**Function signature**:
```rust
pub fn emit_program(
    typed_ast: &TypedAst,
    ty_arena: &TyArena,
) -> Result<BytecodeModule, EmitError>
```

### 4.6 Layer 6: create VM layer

**Operation**: New crate for bytecode execution

**Constraints**: MUST depend only on musi_core and musi_bytecode

**Structure**:
```
musi_vm/
├── src/
│   ├── lib.rs       // Public API
│   ├── vm.rs        // Interpreter
│   ├── value.rs     // Value representation
│   ├── frame.rs     // Stack frames
│   ├── heap.rs      // Object allocation
│   ├── gc.rs        // Garbage collector
│   ├── object.rs    // Heap object types
│   ├── native.rs    // FFI helpers
│   └── capi.rs      // C API exports
├── tests/
│   └── integration.rs
└── capi/
    └── musi_vm.h    // C header
```

**C API implementation**:
```rust
#[no_mangle]
pub extern "C" fn musi_vm_create() -> *mut Vm {
    Box::into_raw(Box::new(Vm::new()))
}

#[no_mangle]
pub extern "C" fn musi_vm_destroy(vm: *mut Vm) {
    unsafe { drop(Box::from_raw(vm)); }
}
```

### 4.7 Layer 7: consolidate driver

**Operation**: Merge CLI and LSP into single driver

**Structure**:
```
musi_driver/
├── src/
│   ├── lib.rs
│   ├── cli.rs           // Command-line interface
│   ├── lsp/
│   │   ├── mod.rs
│   │   ├── server.rs    // LSP server loop
│   │   ├── handlers.rs  // LSP methods
│   │   └── diagnostics.rs
│   └── driver.rs        // Compilation driver
├── tests/
└── Cargo.toml
```

**Dependency chain**:
```
musi_driver → musi_syntax → musi_types → musi_bytecode → musi_vm
                 ↓             ↓           ↓
               musi_source → musi_core ← musi_core (VM only)
```

### 4.8 Cleanup operations

**Crates to remove**:
- `crates/musi_diags/` (8 lines merged into syntax)
- `crates/musi_parser/` (now part of syntax)
- `crates/musi_ast/` (now part of syntax)
- `crates/musi_lex/` (now part of syntax)
- `crates/musi_token/` (now part of syntax)
- `crates/musi_sema/` (split into types and merged)

**Workspace update**:
Edit `Cargo.toml` workspace members:
```toml
[workspace]
members = [
    "crates/musi_core",
    "crates/musi_source",
    "crates/musi_syntax",
    "crates/musi_types",
    "crates/musi_bytecode",
    "crates/musi_vm",
    "crates/musi_driver",
]
```

## 5. Verification Criteria

### 5.1 Dependency Validation

Post-migration, verify acyclic dependency graph:
```bash
$ cargo tree -f "{p} {d}"
musi_driver v0.1.0
├── musi_syntax v0.1.0
│   ├── musi_source v0.1.0
│   │   └── musi_core v0.1.0
│   └── musi_core v0.1.0
├── musi_types v0.1.0
│   ├── musi_syntax v0.1.0 (*)
│   └── musi_core v0.1.0
├── musi_bytecode v0.1.0
│   ├── musi_types v0.1.0 (*)
│   └── musi_core v0.1.0
└── musi_vm v0.1.0
    ├── musi_bytecode v0.1.0 (*)
    └── musi_core v0.1.0
```

**Success criteria**: No diamond dependencies on compiler crates (only core)

### 5.2 Build Time Validation

- Clean build: <30 seconds
- Incremental change (single file): <5 seconds
- Test run: <10 seconds

### 5.3 Testing Requirements

- All existing tests must pass
- New integration tests for layer boundaries
- VM embedding test (C API)
- End-to-end: source → bytecode → execution

### 5.4 Size Constraints

- No layer exceeds 4000 lines
- Average layer size: 1000-2000 lines
- Total lines: ~8,000-10,000 (maintainable)

## 6. Timeline

| Week | Operation | Risk | Verification |
|------|-----------|------|--------------|
| 1 | Purify core, create source | Low | Tests pass |
| 1 | Merge token/ast/lex/parse | Medium | Parse tests |
| 2 | Extract types layer | Medium | Type inference |
| 2 | Create bytecode layer | Low | Bytecode structure |
| 3 | Create VM layer | High | FFI, embedding |
| 3 | Consolidate driver | Low | CLI/LSP work |

Total estimated effort: 80-100 developer-hours

## 7. Rollback Plan

If migration fails:

1. Revert `Cargo.toml` workspace members
2. Restore deleted crate directories from git
3. Revert any modified files
4. Reset to pre-migration commit

**Prerequisite**: Create feature branch `architecture-refactor` before starting.

## 8. References

- [LAYER_MIGRATION.md](./LAYER_MIGRATION.md): Detailed step-by-step instructions
- `/docs/ARCHITECTURE.md`: 7-layer specification
- `/docs/BYTECODE.md`: Bytecode encoding
- `/docs/RUNTIME.md`: VM implementation
