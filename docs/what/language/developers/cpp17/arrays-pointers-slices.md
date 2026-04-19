---
title: "Arrays, Pointers, and Slices"
description: "Read Arrays, Pointers, and Slices as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 6
slug: "arrays-pointers-slices"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

A C++ reader brings habits from RAII, overload sets, templates, references, optional values, exceptions, and object hierarchies. That helps with ownership stories, value categories, and compile-time abstractions, but the Musi page asks a narrower question: what contract should this lists, slices, maps, and pipelines example make visible?

{{compare:cpp17-arrays-pointers-slices}}

## Reading Arrays, Pointers, and Slices from C++17

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi)
