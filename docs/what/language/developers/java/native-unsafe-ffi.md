---
title: "Native, Unsafe, and FFI"
description: "Read Native, Unsafe, and FFI as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 14
slug: "native-unsafe-ffi"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this native and unsafe boundaries example make visible?

{{compare:java-native-unsafe-ffi}}

## Reading Native, Unsafe, and FFI from Java

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
