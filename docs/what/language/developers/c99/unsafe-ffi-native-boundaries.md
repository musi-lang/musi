---
title: "Unsafe, FFI, and Native Boundaries"
description: "Read Unsafe, FFI, and Native Boundaries as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

For C readers this page is not about whether native calls exist; it is about shrinking them. Declare the symbol, call it only under `unsafe`, then return to ordinary Musi values.

{{compare:c99-unsafe-ffi-native-boundaries}}

## Reading Unsafe, FFI, and Native Boundaries from C99

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
