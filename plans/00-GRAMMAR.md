# Grammar Extensions Specification

## Overview

Defines syntactic extensions and production rules for Musi language features beyond base grammar constructs. Prioritizes operator precedence parsing through Pratt techniques rather than traditional LL or LR classification.

## Core Grammar Extensions

### Type Annotations

- User-defined type declarations with parameterization
- Foreign function interface type mappings
- Pointer type syntax with generic type parameters
- Representation attributes for low-level type control
- Compiler intrinsic markers for `Ptr[T]` operations

### Foreign Function Interface Syntax

- External function declaration markers
- C-compatible type qualifiers
- Null pointer constant notation
- String conversion function signatures
- Native call syntax for direct C function invocation
- Type representation specifiers for C ABI compatibility

### Module System Grammar

- Module import and export declarations
- Visibility modifiers and scope delimiters
- Symbol resolution markers for inter-module references
- Foreign linkage declarations for external symbols

### Memory Management Annotations

- Stack allocation directives
- Lifetime qualifiers for transient allocations
- Memory region markers for FFI boundary crossing
- `MemoryStack` API invocation syntax

### Expression Grammar Structure

- Infix operator precedence hierarchies for arithmetic and logical operations
- Prefix unary operators for pointer dereference and address-of operations
- Postfix operators for function call and member access
- Pratt parser binding power definitions for each operator class
- Null coalescing and conditional expression precedence rules

## Design Decisions

Grammar prioritizes explicit type information to enable compile-time verification. Foreign function declarations use dedicated syntax to separate native interface definitions from pure Musi code. Pointer types modeled as generic user-defined types with compiler-recognized semantics. Expression parsing utilizes recursive descent with Pratt parsing for expressions to handle operator precedence without lookahead complexity, avoiding constraints of LL(k) or LR parsing classifications. This approach enables flexible operator definitions and natural precedence handling while maintaining deterministic parsing behavior. Attributes use `:=` not `=` per grammar (attr_arg uses `:=`).

## Pratt Parser Compatibility

### Operator Binding Power Specification

- Left-binding powers for left-associative operators
- Right-binding powers for right-associative operators
- Null-binding powers for prefix and postfix operators
- Binding power resolution for mixed-associativity constructs

### Expression Parsing Strategy

- Prefix parsing functions for unary operators and literals
- Infix parsing functions for binary operators with precedence climbing
- Postfix parsing functions for function calls and member access
- Nud (null denotation) handling for prefix expressions
- Led (left denotation) handling for infix expressions

### Precedence Hierarchy Validation

- Arithmetic operators form precedence levels with proper binding powers
- Logical operators bind with lower precedence than comparison operators
- Pointer operators maintain consistent precedence with address arithmetic
- Function call binds tighter than member access
- Assignment operators bind with lowest precedence

### Grammar Production Constraints

- All expression productions must be expressible as Pratt parsing rules
- Ambiguous token sequences must resolve through binding power differentiation
- Recursive descent structure must accommodate Pratt methodology
- Token lookahead requirements must not exceed single-token peek

## Dependencies

- Base lexical tokenizer for token stream generation
- Core primitive type definitions
- Foreign type mapping registry
- Pratt parser binding power definitions
- Operator precedence hierarchy specifications

## Validation Points

- Grammar must support Pratt parser implementation without lookahead beyond single token
- Foreign type declarations must map to valid C types with compatible representation
- Module declarations must form acyclic dependency graphs
- Memory annotations must be verifiable at compile time
- Operator binding powers must resolve all ambiguities without context
- Expression productions must be parseable through nud/led function pairs
- Prefix and postfix operators must have distinct binding power classifications
- Native function call syntax must integrate with standard expression parsing
