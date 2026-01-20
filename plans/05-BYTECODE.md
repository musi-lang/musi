# Bytecode Emission Specification

## Overview

Defines intermediate representation and code generation process from validated AST to bytecode. Establishes instruction set and emission rules for interpreter. Bytecode architecture provides foreign function interface support through regular `call` instruction with metadata, where VM detects native functions via symbol table and uses C calling convention.

## Instruction Set Architecture

### Core Instructions

- Arithmetic and logical operations
- Control flow and branching
- Stack manipulation primitives
- Variable load and store operations

### Foreign Function Interface Instructions

- Pointer operation intrinsics for `Ptr[T]`
- String conversion and marshaling instructions
- `MemoryStack` API for transient allocation

### Native Function Call Mechanism

- Native functions use regular `call` instruction with metadata
- VM detects native functions via symbol table flags
- Symbol table indicates C calling convention requirement
- No special bytecode instruction for native calls
- `call` instruction operands include symbol reference for resolution

### Pointer Intrinsics

- `Ptr[T]` dereference instruction with type validation
- Address-of instruction for `Ptr[T]` creation
- Pointer arithmetic instruction with bounds checking
- Null pointer check instruction for safety validation
- Pointer cast instruction for type parameter changes
- `Ptr[T]` comparison instruction for equality testing

### Memory Management Instructions

- Allocation and deallocation primitives
- Stack frame management
- Garbage collection control points
- Foreign memory lifecycle operations
- `MemoryStack` push and pop instructions
- `MemoryStack` allocation instruction with size parameter
- `MemoryStack` alignment instruction for C type compatibility

## Code Generation Strategy

### Expression Compilation

- Expression evaluation order preservation
- Temporary value management on stack
- Foreign function call marshaling sequences
- Pointer operation translation to intrinsics

### Native Function Call Code Generation

- Foreign function symbol resolution at compile time
- Type signature extraction for marshaling metadata
- Argument marshaling instruction sequence emission
- Regular `call` instruction emission with symbol reference
- Return value handling instruction sequence generation
- Error handling block generation for exception translation
- Cleanup instruction emission for `MemoryStack` restoration

### Statement Compilation

- Control flow structure generation
- Variable lifetime management
- Module boundary crossing code
- Foreign call error handling sequences

## Bytecode Organization

### Module Structure

- Constant pool for literals and symbols
- Function bytecode segments
- Foreign function linkage table
- Type metadata for runtime verification

### Foreign Function Linkage Table

- Symbol name to address mapping
- Type signature encoding for marshaling
- Calling convention specification
- Library identifier for dynamic linking
- Function pointer caching for repeated calls

### Type Metadata

- C type representation specifications
- `#[repr("C")]` attribute encoding for layout
- `Ptr[T]` type parameter information
- Marshaling rules for type conversions
- Alignment requirements for C compatibility

### Optimization Opportunities

- Constant folding at bytecode level
- Redundant load-store elimination
- Foreign call marshaling optimization
- Stack depth minimization
- `MemoryStack` allocation coalescing for consecutive calls
- String conversion caching for repeated arguments

## Design Decisions

Bytecode uses stack-based execution model for simplicity. Native functions use regular `call` instruction with metadata, where VM detects native functions via symbol table and uses C calling convention. Pointer operations compile to intrinsic sequences validated by type system. `MemoryStack` instructions enable efficient transient allocation with automatic cleanup through scope-based management.

## Dependencies

- Semantic analysis results with type information
- Foreign function interface definitions
- Runtime instruction set specifications
- Module system linkage requirements
- Platform C ABI specifications for calling conventions
- Dynamic linker symbol resolution mechanisms

## Validation Points

- Bytecode must preserve program semantics
- Foreign call marshaling must be type-safe
- Stack depth must be statically computable
- Module boundaries must be enforceable
- Pointer intrinsics must match type system guarantees
- Symbol table must correctly identify native functions
- Argument marshaling must preserve value semantics for primitive types
- Return value handling must correctly transfer ownership for pointers
- `MemoryStack` allocation instructions must maintain proper alignment
- Exception handling blocks must correctly translate C error conditions
- Foreign function linkage table must resolve all symbols at load time
