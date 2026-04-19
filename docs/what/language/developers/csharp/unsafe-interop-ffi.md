---
title: "Unsafe, Interop, and FFI"
description: "Read Unsafe, Interop, and FFI as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 14
slug: "unsafe-interop-ffi"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

A C# reader brings habits from nullable flow analysis, records, LINQ, async tasks, extension methods, interfaces, namespaces, and attributes. That helps with domain code through named APIs and tooling feedback, but the Musi page asks a narrower question: what contract should this native and unsafe boundaries example make visible?

{{compare:csharp-unsafe-interop-ffi}}

## Reading Unsafe, Interop, and FFI from C#

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
