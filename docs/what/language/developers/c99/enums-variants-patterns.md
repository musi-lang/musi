---
title: "Enums, Variants, and Patterns"
description: "Read Enums, Variants, and Patterns as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 9
slug: "enums-variants-patterns"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C reader brings habits from headers, translation units, pointers, arrays, `errno`, sentinel returns, and small functions that trust callers. That helps with cost, layout, and the exact boundary where bytes cross into code, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:c99-enums-variants-patterns}}

## Reading Enums, Variants, and Patterns from C99

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
