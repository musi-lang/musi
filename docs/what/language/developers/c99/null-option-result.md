---
title: "Null, Option, and Result"
description: "Read Null, Option, and Result as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 7
slug: "null-option-result"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A null pointer or `-1` return in C depends on everyone remembering the convention. Musi moves that convention into `Option` so the empty ticket or missing badge cannot be ignored accidentally.

{{compare:c99-null-option-result}}

## Reading Null, Option, and Result from C99

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
