---
title: "Embedding, C API, and FFI"
description: "Read Embedding, C API, and FFI as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 14
slug: "embedding-c-api-ffi"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

Embedding and C APIs are natural to Lua users. Musi keeps native edges possible, but the foreign declaration and unsafe block make the edge audit-friendly.

{{compare:lua-embedding-c-api-ffi}}

## Reading Embedding, C API, and FFI from Lua

On the Musi side, Musi foreign declarations use `foreign "c" let ...` or a parenthesized foreign group, and unsafe calls stay inside `unsafe { ... }`. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not invent a C-like foreign block or spread unsafe assumptions through ordinary Musi code. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use this shape when a clock, driver, C library, host VM, or platform handle must cross into Musi. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
- [Foreign](/learn/book/advanced/foreign)
- [Runtime](/learn/book/effects-runtime/runtime)
