---
title: "Arrays, Pointers, and Slices"
description: "Read Arrays, Pointers, and Slices as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 6
slug: "arrays-pointers-slices"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

C arrays decay, pointer arithmetic hides bounds, and slices are usually a convention. Musi examples keep the collection value and indexed access visible instead of relying on caller discipline.

{{compare:c99-arrays-pointers-slices}}

## Reading Arrays, Pointers, and Slices from C99

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
