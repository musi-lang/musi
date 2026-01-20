# Bootstrap Path Specification

## Overview

Defines progression from initial implementation to self-hosting compiler. Establishes incremental development stages and validation criteria for each phase. Bootstrap strategy prioritizes early foreign function interface implementation to leverage existing C libraries during development.

## Initial Implementation Phase

### Foundation Components

- Minimal parser with core grammar support and Pratt operator parsing
- Basic type system with primitive types and C type representations
- Simple interpreter for core instructions with regular `call` mechanism
- Foreign function interface stub implementation with marshaling support
- `MemoryStack` API implementation for transient allocation

### Parser Implementation Steps

- Token stream generation from lexical analysis
- Expression parsing with Pratt operator binding rules
- Statement parsing with control flow structures
- Type annotation parsing for foreign function declarations
- Module declaration parsing for import and export syntax

### Type System Implementation Steps

- Primitive type definitions for Musi core types
- C type definitions with `#[repr("C")]` attribute support
- `Ptr[T]` generic pointer type with compiler intrinsic recognition
- Foreign function signature type checking
- Basic type inference for expressions

### Interpreter Implementation Steps

- Stack-based bytecode execution engine
- Core arithmetic and logical instruction handlers
- Control flow instruction implementations
- Regular `call` instruction execution with native function detection via symbol table
- `MemoryStack` allocation and deallocation handlers

### Foreign Function Interface Bootstrap

- `CInt`, `CSizeT`, `CChar` type definitions with `#[repr("C")]`
- `C_NILPTR` null pointer constant definition
- Regular `call` instruction with symbol table detection for native functions
- Basic string conversion functions with `MemoryStack` allocation
- `Ptr[T]` compiler intrinsics for `!` and `@` operators
- Dynamic linker integration for foreign symbol resolution

### Validation Criteria

- Parser must handle complete grammar productions with Pratt parsing
- Type system must enforce basic safety rules for foreign types
- Interpreter must correctly execute core operations and native functions via regular `call`
- Foreign function calls must marshal basic types correctly
- `MemoryStack` allocations must clean up on scope exit
- `Ptr[T]` operations must validate type compatibility

## Intermediate Development Phase

### Extended Capabilities

- Complete semantic analysis with module support
- Full type system with generic types and `Ptr[T]` parameterization
- Comprehensive bytecode generation with foreign function marshaling
- Robust interpreter with error handling and exception translation

### Semantic Analysis Implementation

- Symbol table construction with module scoping
- Type checking with foreign function signature validation
- `Ptr[T]` type parameter validation and lifetime inference
- `#[repr("C")]` attribute verification for C compatibility
- Foreign function linkage table generation

### Bytecode Generation Implementation

- Expression compilation with operator precedence preservation
- Regular `call` instruction emission with symbol reference
- `Ptr[T]` intrinsic instruction sequence generation
- `MemoryStack` allocation instruction emission
- Foreign function linkage table encoding

### Module System Implementation

- Module compilation and linking with foreign function resolution
- Import resolution and validation for inter-module references
- Foreign function linkage per module with symbol table for native detection
- `MemoryStack` API integration for transient allocations across modules
- Module boundary validation for type safety

### Validation Criteria

- Modules must compile and link correctly with foreign functions
- Type checking must reject all unsafe constructs including invalid `Ptr[T]` usage
- Bytecode must preserve program semantics including foreign call behavior
- Interpreter must handle all language features with proper exception translation
- `#[repr("C")]` types must match platform C compiler layout exactly
- `MemoryStack` allocations must be correctly scoped and cleaned

## Advanced Features Phase

### Standard Library Development

- Core data type implementations with `Ptr[T]` support
- Container and collection types with foreign memory integration
- I/O and system interface functions using foreign function calls
- Foreign function support utilities with advanced marshaling
- String conversion functions with comprehensive encoding support

### Optimization and Tooling

- Compiler optimization passes for foreign function call sequences
- Profiling and debugging tools for foreign call performance analysis
- Memory management improvements for FFI boundary handling
- Foreign call marshaling optimization with caching strategies
- `MemoryStack` allocation coalescing for efficiency

### Advanced FFI Features

- Callback function registration from Musi to C
- Struct type marshaling with `#[repr("C")]` layout preservation
- Multi-return value simulation through output parameters
- Exception translation refinement with detailed error information
- Pointer ownership transfer semantics for foreign allocations

### Validation Criteria

- Standard library must provide complete functionality with FFI integration
- Tools must enable effective development workflow for foreign code
- Performance must meet target benchmarks including foreign call overhead
- Foreign function overhead must be acceptable for production use
- Callback mechanisms must correctly handle Musi function references
- Struct marshaling must preserve exact C memory layout

## Self-Hosting Transition

### Compiler in Musi Implementation

- Parser implementation in Musi using Pratt parsing techniques
- Type system implementation in Musi with `#[repr("C")]` and `Ptr[T]` support
- Bytecode emitter implementation in Musi with regular `call` generation
- Interpreter implementation in Musi with foreign call execution via symbol table
- Foreign function interface implementation in Musi for self-compilation

### Self-Hosting Prerequisites

- All language features must be compilable in Musi
- Foreign function interface must be fully functional in Musi
- `Ptr[T]` intrinsics must be available for compiler implementation
- `MemoryStack` API must be usable for compiler transient allocations
- String conversion must work for compiler internal operations

### Self-Hosted Validation

- Self-compiled compiler must produce identical output to bootstrap compiler
- Performance of self-compiled compiler must be acceptable for development
- All language features must be self-compilable including FFI constructs
- Foreign function interface must be fully functional in self-compiled version
- `#[repr("C")]` types must work correctly in self-compiled compiler
- Regular `call` instruction must execute correctly for native functions in self-compiled runtime

### Self-Hosting Verification Steps

- Compile compiler source with bootstrap compiler
- Execute self-compiled compiler on same source
- Compare bytecode output byte-for-byte with bootstrap compiler output
- Verify foreign function linkage tables match exactly
- Test `Ptr[T]` operations in self-compiled compiler
- Validate `MemoryStack` behavior in self-compiled runtime
- Performance benchmark comparison between compilers

## Design Decisions

Bootstrap proceeds from minimal viable implementation to complete language. Foreign function interface implemented early to enable C library access during development. Self-hosting achieved only after all features are stable. Native functions use regular `call` mechanism throughout with symbol table detection, no special bytecode instructions needed for FFI. `Ptr[T]` with compiler intrinsics enables type-safe pointer manipulation for compiler implementation. `MemoryStack` API provides efficient transient allocation for compiler operations. `#[repr("C")]` attribute ensures exact C compatibility for foreign types.

## Dependencies

- All preceding specification components
- C compiler for foreign function support
- Operating system for runtime environment
- Test suites for validation at each phase
- Dynamic linker for foreign symbol resolution
- Platform C ABI specifications for calling conventions
- Unicode handling libraries for string conversion

## Validation Points

- Each phase must meet completion criteria before advancing
- Self-compiled compiler must be functionally equivalent to bootstrap compiler
- Foreign function interface must work at all phases of bootstrap
- Performance must not regress during transition to self-hosting
- `Ptr[T]` operations must work correctly in self-compiled compiler
- `#[repr("C")]` types must maintain exact C compatibility throughout bootstrap
- `MemoryStack` allocations must be correctly scoped in all phases
- Regular `call` instruction must execute correctly for native functions in both bootstrap and self-compiled versions
- String conversion must handle all Unicode codepoints in self-compiled compiler
- Exception translation must preserve error information across self-hosting boundary
