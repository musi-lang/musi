# Parser Architecture Specification

## Overview

Defines parsing pipeline from source text to abstract syntax tree, including lexical analysis, syntax validation, and structural transformation phases.

## Lexical Analysis Layer

### Token Classification

- Primitive type tokens including `CInt`, `CSizeT`, `CChar`
- Foreign interface markers and attribute tokens
- Operator and delimiter token definitions
- Identifier and literal token patterns

### Token Stream Properties

- Position tracking for error reporting
- Whitespace and comment handling
- Unicode identifier support

## Syntax Analysis Layer

### Parsing Strategy

- Recursive descent parsing for production rules
- Operator precedence and associativity resolution
- Type annotation parsing and validation
- Foreign function declaration syntax handling

### AST Node Types

- Module and namespace declarations
- Type definitions including UDT with `#[repr("C")]`
- Function declarations with foreign markers
- Expression and statement nodes

## Error Recovery

### Error Detection

- Syntax error identification and localization
- Missing token insertion for common patterns
- Invalid construct detection with context

### Recovery Mechanisms

- Synchronization points at statement boundaries
- Partial AST construction for error reporting
- Cascade error prevention

## Design Decisions

Parser produces typed AST nodes to reduce semantic analysis burden. Foreign function declarations receive special AST markers for later FFI processing. Error recovery prioritizes continued parsing for maximal error reporting.

## Dependencies

- Grammar specification production rules
- Token type definitions and constants
- AST node structure definitions

## Validation Points

- Parser must handle all grammar productions deterministically
- AST must preserve source location information
- Error recovery must not produce malformed trees
- Foreign function markers must be correctly identified
- Type annotations must be syntactically validated
