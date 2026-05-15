# Musi C Interop Memory Mapping

Status: proposed

This spec defines how Musi's managed-core memory model meets C ABI concepts.

## Rust Comparison

Rust 2024 FFI is the host implementation reference for ABI calls, `unsafe extern`, raw pointer handling, and ownership boundaries.

Musi does not expose Rust FFI syntax as the language model. Musi boundary types stay explicit:

- `CString` for C-compatible string mapping
- `CPtr` for erased C pointer mapping
- `Ptr[T]` for typed raw pointer wrappers
- `Maybe[...]` or fallible `E!T` for nullable/error results

Rust may enforce wrapper invariants in implementation code; Musi wrapper APIs must still document nullability, ownership, aliasing, and lifetime consequences in Musi terms.

## Boundary Types

Current repo exports:

- `CString`
- `CPtr`

from `crates/musi_foundation/modules/core.ms` and `lib/std/prelude.ms`.

These names remain valid, but their role is narrow:

- `CString` maps C string concepts into Musi
- `CPtr` maps untyped C pointer concepts into Musi

They are not the primary source-language address model.

Primary source-language raw pointer type is `Ptr[T]`.

## Mapping Rules

### `CString`

`CString` means C-compatible string boundary value.

Rules:

- Musi `String` does not imply C string layout
- conversion between `String` and `CString` is explicit
- null C string results map explicitly, not silently
- invalid UTF-8 stays interop error, not ambient language behavior

### `CPtr`

`CPtr` means untyped machine pointer for C interop.

Rules:

- use `CPtr` when C surface is untyped or erased
- prefer `Ptr[T]` in Musi-native unsafe wrappers
- convert between `CPtr` and `Ptr[T]` explicitly under unsafe context

## Managed Object Interop

Because managed Musi objects may move, code must not hand raw addresses of managed storage to C unless one of these holds:

- object or buffer is pinned for exact call scope
- interop layer copies data into C-owned or pinned temporary storage
- runtime object is already foreign-owned and outside managed movement rules

Pinned managed addresses must not outlive lexical pin scope.

## Nullability And Results

C nulls do not infect Musi type defaults.

Mapping policy:

- nullable C pointer result => `Maybe[Ptr[T]]` or `Maybe[CPtr]`
- nullable C string result => `Maybe[CString]` if absence is ordinary result
- null where contract forbids it => interop failure

Choice between optional result and interop failure belongs to wrapper API contract, not ambient pointer semantics.

## Unsafe Requirements

These operations require unsafe context:

- dereferencing `Ptr[T]`
- converting `CPtr` to typed pointer
- extracting raw address from pinned managed storage
- calling foreign APIs that require caller-side alias, lifetime, or layout invariants

These operations do not require unsafe by themselves if wrapped safely:

- ordinary `String` use
- ordinary `Array[T]` use
- safe wrapper calls whose invariants are enforced inside wrapper

## Wrapper Policy

Std and foundation should prefer safe wrappers around raw C boundary shapes.

Pattern:

- narrow unsafe leaf
- typed wrapper around it
- Musi-facing API avoids `CPtr` where stronger type exists

This follows same principle as D `@safe` / `@trusted` layering and C# safe-by-default code with explicit unsafe islands.

## References

- C# unsafe contexts: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/unsafe
- Digital Mars article list: https://digitalmars.com/articles/index.html
- D language specification index: https://dlang.org/spec/spec.html
- Go memory model: https://go.dev/ref/mem
