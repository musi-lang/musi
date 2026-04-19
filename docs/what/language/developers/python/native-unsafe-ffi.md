---
title: "Native Boundaries, Unsafe, and FFI"
description: "Read Native Boundaries, Unsafe, and FFI as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 13
slug: "native-unsafe-ffi"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

A Python reader brings habits from names, dictionaries, classes, protocols, exceptions, None, imports, and duck typing. That helps with code around domain words before machinery, but the Musi page asks a narrower question: what contract should this native and unsafe boundaries example make visible?

{{compare:python-native-unsafe-ffi}}

## Reading Native Boundaries, Unsafe, and FFI from Python

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
