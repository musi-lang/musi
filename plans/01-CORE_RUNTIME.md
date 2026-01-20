# Core Runtime Specification

## Overview

Defines foundational runtime environment including memory management, value representation, and execution primitives. Establishes execution model for all language features. Runtime architecture provides explicit foreign function interface support with C#-inspired native call mechanisms and managed pointer abstractions.

## Value Representation Model

### Primitive Value Encoding

- Integer and floating-point representations aligned with C ABI
- Boolean values as single-byte flags
- Null pointer sentinel value `C_NILPTR`
- Unicode string encoding and internal format
- C type representations with `#[repr("C")]` attribute semantics

### Pointer Handling

- `Ptr[T]` type as memory address wrapper with compiler intrinsic support
- Pointer arithmetic rules and bounds checking
- Null pointer validation and safety checks
- Pointer type parameterization with compile-time type verification
- `Ptr[T]` dereference semantics with runtime type validation
- Address-of operator for obtaining `Ptr[T]` from managed references

### C Type Representation

- `CInt` type mapping to platform C integer representation
- `CSizeT` type mapping to platform C size_t representation
- `CChar` type mapping to platform C character representation
- `#[repr("C")]` attribute for explicit C-compatible layout specification
- Type alignment requirements matching platform C compiler defaults

## Memory Management Architecture

### Allocation Strategies

- Automatic garbage collection for managed objects
- Stack-allocated transient buffers via `MemoryStack` API
- Foreign-allocated memory lifecycle tracking
- Hybrid allocation model supporting both managed and unmanaged memory

### MemoryStack API Specification

- Stack-like allocation region for transient FFI buffers
- Allocation pointer tracking with push and pop operations
- Automatic cleanup on scope exit via RAII semantics
- Minimum allocation alignment for C type compatibility
- Allocation size tracking for overflow prevention
- Nested allocation support for nested foreign call sequences
- Stack pointer restoration on exception unwinding

### Memory Safety Guarantees

- Reference counting for shared objects
- Lifetime tracking for stack-allocated data
- `MemoryStack` allocation bounds validation
- Foreign memory ownership tracking to prevent double-free
- Pointer validity verification before dereference operations

## Foreign Function Interface Runtime

### Call Conventions

- Native call bytecode instruction semantics
- Argument marshaling between Musi and C representations
- Return value translation and error handling
- C ABI compatibility for platform-specific calling conventions
- Register and stack-based argument passing support
- Return value register handling for primitive and pointer types

### Argument Marshaling Mechanism

- Primitive type direct pass-through for C-compatible types
- `Ptr[T]` unwrapping to raw pointer addresses
- String conversion to C-string via temporary `MemoryStack` allocation
- Array pointer extraction with length parameter marshaling
- Struct type marshaling with field-by-field conversion
- Callback function pointer registration and lifetime management
- Exception translation from C error codes to Musi exceptions

### Return Value Handling

- Primitive return value direct conversion to Musi types
- Pointer return value wrapping into `Ptr[T]` with type annotation
- C-string return value conversion to Musi string with memory transfer
- Void function return handling with runtime continuation
- Error code detection and exception raising pattern
- Multi-return value simulation through output parameter marshaling

### String Interoperability

- Musi string to C-string conversion functions
- Temporary buffer allocation for C string arguments
- Encoding validation and transformation
- UTF-8 to platform encoding conversion as required
- C-string lifetime management tied to `MemoryStack` scope
- String length calculation for C string parameter marshaling
- Null-terminated string generation for C compatibility

### Native Call Execution Flow

- Foreign function symbol resolution via dynamic linker
- Stack frame preparation with argument marshaling
- C function invocation with platform-specific calling convention
- Return value extraction and Musi type reconstruction
- Exception detection and translation to Musi exception system
- Runtime invariant restoration after foreign call completion
- GC state management across foreign call boundaries

## Design Decisions

Runtime maintains explicit separation between managed and unmanaged memory. Foreign calls use dedicated marshaling layer to prevent memory corruption. Stack allocation reserved for transient FFI arguments to reduce GC pressure. `Ptr[T]` provides type-safe pointer abstraction with compiler intrinsic support for common operations. `MemoryStack` API enables efficient transient allocation with automatic cleanup through scope-based lifetime management.

## Dependencies

- Operating system memory allocation primitives
- C standard library for FFI operations
- Platform-specific type definitions
- Dynamic linker for foreign symbol resolution
- Unicode handling libraries for string conversion
- Platform C ABI specifications for calling conventions

## Validation Points

- Memory allocation must be exception-safe
- Foreign calls must preserve runtime invariants
- String conversion must handle all Unicode codepoints
- Pointer operations must validate alignment and bounds
- `MemoryStack` allocations must be automatically cleaned on scope exit
- `Ptr[T]` dereferencing must verify type compatibility at runtime
- Native call marshaling must preserve type safety across language boundaries
- C type representations with `#[repr("C")]` must match platform C compiler output
- Argument marshaling must maintain value semantics for primitive types
- Return value translation must correctly handle pointer ownership transfer
- Exception translation must preserve error information across boundary
- GC must correctly track references held by foreign code
