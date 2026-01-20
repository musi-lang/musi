# Type System Design Specification

## Overview

Defines static type system including type inference, checking, and foreign type compatibility rules. Establishes type safety guarantees and representation mappings.

## Type Hierarchy

### Primitive Types

- Integer and floating-point numeric types
- Boolean and character types
- Foreign C types: `CInt`, `CSizeT`, `CChar` with `#[repr("C")]` attribute
- Null pointer constant `C_NILPTR` type

### Composite Types

- User-defined types with representation attributes
- Generic pointer type `Ptr[T]` as UDT
- Module-level type parameterization
- Function types with foreign call markers

## Type Checking Rules

### Subtyping and Compatibility

- Numeric type promotion and conversion rules
- Foreign type compatibility with C ABI types
- Pointer type covariance and contravariance
- Module type visibility and accessibility

### Foreign Function Interface Types

- C-compatible type representations via `#[repr("C")]`
- Pointer type intrinsics for `Ptr[T]` operations
- String type conversion compatibility
- Null pointer safety constraints

## Type Inference

### Inference Algorithm

- Local variable type inference from expressions
- Generic type parameter resolution
- Foreign function return type inference
- Module-level type propagation

### Inference Constraints

- Ambiguity resolution rules
- Foreign type mapping constraints
- Representation attribute validation

## Design Decisions

Type system provides explicit foreign type annotations to enable compile-time FFI validation. Pointer types modeled as UDTs with compiler-recognized semantics for type checking. Generic type system supports module-level abstraction.

## Dependencies

- Grammar type annotation syntax
- Foreign type mapping definitions
- Runtime value representation model

## Validation Points

- Type checking must reject unsafe foreign type conversions
- Generic type resolution must be decidable
- Foreign type representations must match C ABI
- Pointer operations must be type-safe
- Module type boundaries must be enforceable
