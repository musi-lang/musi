---
title: "Unsafe and FFI"
description: "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "unsafe-and-ffi"
summary: "Keep raw pointer and native-call work behind a visible unsafe boundary."
---

Native code can do things the Musi type system cannot prove safe. Musi marks that boundary with `unsafe { ... }`.

{{snippet:chapter-unsafe-and-ffi}}

`unsafe` is a block because it is a structural scope. Everything inside the braces is checked normally, but calls marked unsafe are only allowed while the checker is inside that scope. The last expression of the block is still the block result.

## C ABI Types

Use `CString` for null-terminated C strings and `CPtr` for raw C pointer values in native signatures. Use fixed-width numeric types (`Int8` through `Int64`, `Nat8` through `Nat64`, `Float32`, `Float64`) when the ABI requires an exact size. `Int`, `Nat`, and `Float` are language-sized defaults; `Float` follows the compiler target `fsize` policy.

{{snippet:ffi-c-abi-signatures}}

`CPtr` means "this value has C pointer ABI shape." It is the right type at the native boundary. `@std/ffi` adds typed pointer views for Musi code that wants to name what a raw pointer points at after the boundary has already been crossed.

`@std/ffi` C aliases map onto those core numeric types: `CInt` is `Int32`, `CUInt` is `Nat32`, `CFloat` is `Float32`, and `CDouble` is `Float64`. Host-dependent aliases such as `CLong`, `CULong`, `CSize`, and `CSizeDiff` follow the active C ABI.

{{snippet:ffi-typed-pointer-view}}

A `Ptr[T]` is a typed view over a `CPtr`. It does not replace `CPtr` in native declarations. Use `CPtr` at the C boundary, then use `Ptr[T]` when Musi code needs a clearer static name for the pointed-at data.

`Ffi.ptr.null[T]()` creates a typed null pointer. `Ffi.ptr.isNull[T](pointer)` checks it before dereference.

`Ffi.ptr.offset[T](pointer, count)` moves by elements, not bytes. For `Ptr[CInt]`, `count := 2` means two C ints.

`Ffi.ptr.read[T](pointer)` and `Ffi.ptr.write[T](pointer, value)` dereference process memory. Calls to `offset`, `read`, and `write` belong inside `unsafe { ... }` because Musi cannot prove the address is valid.

Use `CPtr` when declaring native calls. Use `Ptr[T]` after the call when Musi code needs to state the pointed-at shape.

`Ffi.ptr` is an ordinary record of values. The pointer operations are first-class polymorphic fields, so an alias keeps the type parameter.

The unsafe rule follows the value. Calling `offset[Int]` through an alias still needs `unsafe { ... }`.

Use `@std/os` for typed platform checks and `@std/sys` for raw target facts when a native wrapper needs platform-specific declarations. `@when` compares those facts case-insensitively, so `aarch64`, `AArch64`, and mixed-case forms match the same target. Rust-supported targets define the platform universe. Future Cranelift JIT support is exposed separately through `@std/sys.jit` and `@std/os.Jit`, not through ordinary OS support.

{{snippet:unsafe-safe-wrapper}}

## Wrapper Shape

Raw native work should stay explicit through `unsafe { ... }`, `CPtr`, and the `@std/ffi` helpers. Musi also keeps the final expression as the value inside unsafe blocks, just like any other block.

Do not hide a native call in ordinary code and expect the reader to notice it from the function name. Put the call inside a small unsafe block, then expose a safe wrapper when the rest of the module should not care about native details.

Continue to [Operator Forms](/learn/book/advanced/meta-tooling/operator-forms).
