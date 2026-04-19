---
title: "Arrays and Data Pipelines"
description: "Read Arrays and Data Pipelines as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 5
slug: "arrays-pipelines"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

A TypeScript reader brings habits from objects, structural types, unions, undefined, promises, imports, npm packages, and prototype-shaped history. That helps with flexible shapes and fast feedback from type tooling, but the Musi page asks a narrower question: what contract should this lists, slices, maps, and pipelines example make visible?

{{compare:javascript-typescript-arrays-pipelines}}

## Reading Arrays and Data Pipelines from JavaScript/TypeScript

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Dot calls](/learn/book/core/dot-calls)
