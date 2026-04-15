---
title: "Unsafe and FFI"
description: "Translate Rust unsafe and extern C habits into Musi foreign declarations and unsafe blocks."
group: "Musi for Developers"
section: "Rust Developers"
order: 10
slug: "unsafe-ffi"
summary: "Rust `extern` and raw pointer work maps to Musi `foreign`, `CPtr`, `@std/ffi`, and `unsafe`."
---

Rust marks C ABI declarations and raw pointer calls explicitly:

```rust
unsafe extern "C" {
    fn get_counter() -> *mut i32;
}

let counter = unsafe { get_counter() };
```

Musi keeps the same boundary visible with `foreign` and `unsafe`.

{{snippet:rust-unsafe-ffi}}

Use `foreign` for the ABI declaration. Use `unsafe` for the block where raw native assumptions matter. Use `@std/ffi` helpers when pointer views need typed reads, writes, offsets, or casts.
