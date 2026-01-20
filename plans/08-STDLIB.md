# Standard Library Specification

## Overview

Defines core library of built-in functions and types providing essential functionality. Establishes baseline capabilities available to all programs without external dependencies. Standard library provides comprehensive foreign function interface support with C#-inspired pointer abstractions and compiler intrinsic operations.

## Core Data Types

### Primitive Operations

- Integer and floating-point arithmetic functions
- Comparison and equality operations
- Boolean logic and bit manipulation
- Character and string primitive operations

### Container Types

- Array and list operations
- Dictionary and map functions
- Set operations and membership testing
- Iteration and traversal primitives

## Foreign Function Interface Support

### C Type Definitions

- `CInt` type declaration with `#[repr("C")]` for platform C integer
- `CSizeT` type declaration with `#[repr("C")]` for platform C size_t
- `CChar` type declaration with `#[repr("C")]` for platform C character
- `C_NILPTR` null pointer constant for pointer initialization
- `#[repr("C")]` attribute semantics for C-compatible type layout
- Type alignment specifications matching platform C compiler
- Type size guarantees for C ABI compatibility

### repr C Attribute Specification

- Explicit C-compatible layout control for type definitions
- Field ordering preservation matching C struct semantics
- Padding and alignment rules matching platform C compiler
- Bit-field support for C-compatible integer packing
- Union type support for C-compatible memory overlay
- Enum type mapping to C integer representation
- Endianness handling for cross-platform C compatibility

### Ptr[T] Pointer Type

- Generic pointer type parameterized by pointed-to type T
- Compiler intrinsic support for common pointer operations
- Type-safe pointer dereferencing with runtime validation
- Pointer arithmetic with bounds checking for array types
- Null pointer detection and safety semantics
- Pointer comparison operations for equality and ordering
- Pointer to pointer nesting for multi-level indirection
- `Ptr[T]` intrinsics enable `!` (dereference) and `@` (address-of) operators

### Ptr[T] Compiler Intrinsics

- `!` operator for dereference intrinsic with type validation
- `@` operator for address-of intrinsic from managed references
- Pointer offset intrinsic for arithmetic with size-based scaling
- Null check intrinsic for safety validation before dereference
- Pointer cast intrinsic for type parameter changes with validation
- Pointer to integer conversion intrinsic for address manipulation
- Integer to pointer conversion intrinsic for address reconstruction

### Ptr[T] Type System Integration

- Generic type parameter validation for pointer targets
- Covariance and contravariance rules for pointer types
- Subtype compatibility for pointer assignment and comparison
- Lifetime inference for pointer validity tracking
- Ownership semantics for pointer return from foreign functions
- Borrow checking for pointer usage within safe contexts

### Foreign Type Compatibility Utilities

- C type to Musi type mapping functions
- Type size and alignment query functions
- Type marshaling and unmarshaling helpers
- C struct definition translation utilities
- C enum definition translation utilities
- C function signature matching helpers

### String Conversion Functions

- Musi to C-string conversion utilities
- C-string to Musi string conversion
- Encoding validation and transformation
- Temporary buffer management helpers
- C-string null-termination guarantee functions
- String length calculation for C string parameters
- String encoding detection and conversion functions

### Memory Management API

- `MemoryStack` for stack-allocated transient strings
- Buffer allocation and deallocation
- Lifetime management for FFI arguments
- Memory safety validation functions
- `MemoryStack` scope management for nested allocations
- Alignment-aware allocation functions for C type compatibility
- Bounds checking functions for pointer arithmetic validation

## Input and Output

### Stream Operations

- Standard input and output streams
- File reading and writing functions
- Text and binary stream handling
- Buffer management for I/O operations

### String Formatting

- String interpolation and formatting
- Numeric and type conversion to string
- Unicode handling and encoding support
- String manipulation primitives

## System Interfaces

### Runtime Information

- Environment variable access
- Command-line argument retrieval
- System information queries
- Runtime configuration management

### Error Handling

- Exception throwing and catching utilities
- Error type definitions and hierarchies
- Stack trace inspection functions
- Recovery and cleanup helpers

## Design Decisions

Standard library avoids external C library dependencies beyond FFI support. String conversion functions provide safe Musi to C-string bridging. `MemoryStack` API enables efficient transient allocations for foreign calls. `Ptr[T]` provides C#-inspired type-safe pointer abstraction with compiler intrinsic support. `#[repr("C")]` attribute gives explicit control over C-compatible type layout for FFI compatibility. Compiler intrinsics for `Ptr[T]` operations enable efficient pointer manipulation while maintaining type safety through runtime validation. `Ptr[T]` intrinsics enable `!` and `@` operators for dereference and address-of operations.

## Dependencies

- Core runtime value representations
- Foreign function interface definitions
- Operating system primitives for system calls
- Unicode handling libraries for text operations
- Platform C compiler for `#[repr("C")]` attribute verification
- Dynamic linker for foreign symbol resolution

## Validation Points

- All library functions must have type-safe interfaces
- Foreign function wrappers must preserve invariants
- String conversion must handle all Unicode codepoints
- Memory management must prevent leaks and corruption
- Error handling must provide useful diagnostic information
- `Ptr[T]` dereferencing must validate type compatibility at runtime
- `#[repr("C")]` types must match platform C compiler layout exactly
- Compiler intrinsics must be recognized and handled by compiler
- `!` and `@` operators must work correctly for `Ptr[T]` dereference and address-of
- Pointer arithmetic must maintain bounds checking for array types
- `MemoryStack` allocations must be automatically cleaned on scope exit
- C type mappings must preserve value semantics across boundary
- Null pointer checks must prevent unsafe dereferences
