# Musi Compiler AI Coding Agent Instructions

## Project Overview

Musi is an interpreted systems language emphasizing semantic correctness and context-free parsing. The compiler is written in Rust and produces MSIL bytecode for a mark-sweep GC runtime.

**Core Architecture:**

- `musi_basic`: Common types, spans, diagnostics, source mapping
- `musi_lexer`: Tokenization
- `musi_parser`: Parser (produces untyped AST)
- `musi_ast`: AST definitions (untyped and typed, mutated during sema)
- `musi_types`: Type system foundation, union-find
- `musi_sema`: Type inference, name resolution (mutates AST with type info)
- `musi_codegen`: Bytecode emission to `.mso` format
- `musi_runtime`: MSIL interpreter with mark-sweep GC
- `musi_lsp`: Language Server Protocol implementation
- `musi_cli`: Command-line interface orchestrating all phases

## Build System

### Cargo Commands

```bash
# Build workspace
cargo build

# Run compiler
cargo run --bin musi -- input.ms

# Run tests
cargo test

# Run clippy
cargo clippy

# Format code
cargo fmt
```

## Code Conventions

### File Organization

Every module follows this ordering:

1. **Imports** - Grouped by std, external crates, workspace crates
2. **Types** - Structs, enums, type aliases
3. **Constants** - Module-level constants
4. **Public functions** - API surface
5. **Private helpers** - Implementation details
6. **Tests** - In `tests.rs` submodule or `#[cfg(test)]`

### Documentation Style

- Use `///` doc comments for public API only
- Use `//` for section separators and implementation notes
- NO inline comments explaining obvious code
- NO `TODO`, `FIXME`, `unimplemented!()`, `todo!()`, or placeholder markers

### Naming Conventions

| Category | Pattern | Example |
|----------|---------|---------|
| Functions | `snake_case` | `compile_program`, `check_types` |
| Types | `PascalCase` | `Lexer`, `Parser`, `TypeEnv` |
| Constants | `SCREAMING_SNAKE_CASE` | `MAX_STACK_SIZE`, `DEFAULT_HEAP_THRESHOLD` |
| Type Parameters | `PascalCase` (single letter) | `T`, `E`, `K`, `V` |
| Files | `snake_case.rs` | `token.rs`, `source.rs` |

## Design Principles

### DRY (Don't Repeat Yourself)

- Extract common patterns into functions or macros
- Share types and utilities through `musi_basic`
- Avoid duplicating logic across crates

### SRP (Single Responsibility Principle)

- Each function has one clear purpose
- Each struct represents one concept
- Each module handles one phase or concern

### KISS (Keep It Simple, Stupid)

- Prefer straightforward solutions over clever ones
- Write code that does what it needs to do
- Complex algorithms require documentation explaining necessity

### No Placeholder Code

Forbidden in production:

- "for now", "temporary"
- `TODO`, `FIXME`
- "in a real implementation", "simplified version"
- Commented-out code blocks
- `todo!()`, `unimplemented!()`, `unreachable!()`

## Architecture Patterns

### Compilation Pipeline

```text
Source (.ms) → musi_lexer (tokens) → musi_parser (untyped AST)
              → musi_sema (mutates AST with types, produces errors MS3xxx/MS4xxx)
              → musi_codegen (MSIL bytecode .mso)
              → musi_runtime (execution, runtime errors MS7xxx)
```

### Error Handling

- Return `Result<T, E>` for fallible operations
- Use `?` operator for error propagation
- Never use `panic!()` or `unwrap()` in production
- `expect()` permitted only with documented invariants
- Use `thiserror` for error type definitions

### Memory Management

- Use arena allocation for AST nodes and type information
- Type IDs are generational indices, not lifetimes
- Prefer indices over references in hot paths
- Pre-allocate `Vec` capacity when size is known

## Error Codes

Musi uses MS-prefixed error codes:

- **MS1xxx**: Lexer errors
- **MS2xxx**: Parser errors
- **MS3xxx**: Type errors
- **MS4xxx**: Semantic errors
- **MS7xxx**: Runtime errors

**Implementation**:

- `musi_sema` generates diagnostics with MS codes
- `musi_cli` displays messages without codes for terminal
- `musi_lsp` preserves codes for editor diagnostics

## Module Dependencies

```text
musi_basic → musi_lexer → musi_parser → musi_ast → musi_sema → musi_codegen → musi_runtime
                ↓            ↓          ↓           ↓              ↗
            musi_types ──────┴──────────┴───────────┴──────────────┘
                                                     ↓
                                                 musi_lsp
                                                     ↓
                                                 musi_cli
```

## Key Files

- `grammar.ebnf`: EBNF grammar specification
- `musi_basic/src/span.rs`: Span and source location tracking
- `musi_ast/src/lib.rs`: AST node definitions
- `musi_sema/src/lib.rs`: Type checker and semantic analyzer
- `musi_runtime/src/lib.rs`: MSIL interpreter and GC

## Common Pitfalls

1. **Span tracking**: Always preserve spans through all phases for diagnostics
2. **AST mutation**: `musi_sema` mutates the AST from `musi_parser` with type annotations
3. **Arena allocation**: Use indices, not references with lifetimes
4. **Error propagation**: Use `?`, not `unwrap()` or `expect()` without justification
5. **Clippy lints**: All categories set to `deny`, no suppressions in production code
