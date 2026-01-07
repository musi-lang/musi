# musi_core

Core primitives for Musi compiler.

This crate provides foundational types used throughout compiler pipeline:

- **Span**: Source code locations
- **Source**: Source file management
- **Intern**: String interning
- **Arena**: Generic arena allocation
- **Token**: Lexical tokens
- **Symbol**: Interned identifiers with spans
- **Diag**: Diagnostic infrastructure
- **Error**: Error types and result aliases

## Design Principles

- Zero dependencies on other Musi crates
- Minimal external dependencies
- All types are `Send + Sync` safe where possible
