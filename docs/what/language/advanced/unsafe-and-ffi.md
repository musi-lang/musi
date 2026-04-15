---
title: "Unsafe and FFI"
description: "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 32
slug: "unsafe-and-ffi"
summary: "Keep raw pointer and native-call work behind a visible unsafe boundary."
---

Native code can do things the Musi type system cannot prove safe. Musi marks that boundary with `unsafe { ... }`.

`unsafe` is a block because it is a structural scope. Everything inside the braces is checked normally, but calls marked unsafe are only allowed while the checker is inside that scope. The last expression of the block is still the block result.

{{snippet:chapter-unsafe-and-ffi}}

## C ABI types

Use `CString` for null-terminated C strings and `CPtr` for raw C pointer values in native signatures.

The same boundary model applies to declarations such as `puts` for `CString` and `memset` for `CPtr`.

`CPtr` means "this value has C pointer ABI shape". It is the right type at the native boundary.

`@std/ffi` adds typed pointer views for Musi code that wants to name what a raw pointer points at after the boundary has already been crossed.

`@std/ffi` then lets Musi code cast a `CPtr` into a typed `Ptr[T]` view after the call has crossed the native boundary.

A `Ptr[T]` is a typed view over a `CPtr`. It does not replace `CPtr` in native declarations. Use `CPtr` at the C boundary, then use `Ptr[T]` when Musi code needs a clearer static name for the pointed-at data.

`Ffi.ptr.null[T]()` creates a typed null pointer. `Ffi.ptr.isNull[T](pointer)` checks it before dereference.

`Ffi.ptr.offset[T](pointer, count)` moves by elements, not bytes. For `Ptr[CInt]`, `count := 2` means two C ints.

`Ffi.ptr.read[T](pointer)` and `Ffi.ptr.write[T](pointer, value)` dereference process memory. Calls to `offset`, `read`, and `write` belong inside `unsafe { ... }` because Musi cannot prove the address is valid.

Use `CPtr` when declaring native calls. Use `Ptr[T]` after the call when Musi code needs to state the pointed-at shape.

`Ffi.ptr` is an ordinary record of values. The pointer operations are first-class polymorphic fields, so an alias keeps the type parameter:

```text
let offset := Ffi.ptr.offset;

let next := unsafe {
  offset[Int](counter, 1);
};
```

The unsafe rule follows the value. Calling `offset[Int]` through an alias still needs `unsafe { ... }`.

## What Musi does not have

Musi does not use a `borrow` keyword or pointer sigils for this model. Raw native work should stay explicit through `unsafe { ... }`, `CPtr`, and the `@std/ffi` helpers.

Musi also does not use `return` inside unsafe blocks. The final expression is the value, just like any other block.

## Common mistake

Do not hide a native call in ordinary code and expect the reader to notice it from the function name. Put the call inside a small unsafe block, then expose a safe wrapper when the rest of the module should not care about native details.

A wrapper can keep the unsafe block small while exposing an ordinary Musi function to the rest of the module.

## Next

Continue to [Running and tooling](/docs/language/advanced/running-and-tooling) for command workflow and validation.
