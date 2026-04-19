---
title: "Null, Undefined, Option, and Result"
description: "Read Null, Undefined, Option, and Result as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 6
slug: "null-result"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

Undefined, optional properties, and nullable unions are common TypeScript habits. Musi uses `someOf` and `noneOf` so expected absence has one visible shape in examples.

{{compare:javascript-typescript-null-result}}

## Reading Null, Undefined, Option, and Result from JavaScript/TypeScript

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
