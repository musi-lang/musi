---
title: "Unsafe and FFI"
description: "Read Unsafe and FFI as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 10
slug: "unsafe-ffi"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

Rust unsafe marks code the compiler cannot fully protect. Musi uses the same warning shape: declare foreign symbols, then keep calls inside `unsafe { ... }`.

{{compare:rust-unsafe-ffi}}

## Reading Unsafe and FFI from Rust

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
