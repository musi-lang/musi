---
title: "Unsafe, FFI, and Native Boundaries"
description: "Read Unsafe, FFI, and Native Boundaries as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

A C++ reader brings habits from RAII, overload sets, templates, references, optional values, exceptions, and object hierarchies. That helps with ownership stories, value categories, and compile-time abstractions, but the Musi page asks a narrower question: what contract should this native and unsafe boundaries example make visible?

{{compare:cpp17-unsafe-ffi-native-boundaries}}

## Reading Unsafe, FFI, and Native Boundaries from C++17

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
