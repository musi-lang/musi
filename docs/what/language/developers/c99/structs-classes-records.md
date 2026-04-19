---
title: "Structs, Classes, and Records"
description: "Read Structs, Classes, and Records as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 5
slug: "structs-classes-records"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C reader brings habits from headers, translation units, pointers, arrays, `errno`, sentinel returns, and small functions that trust callers. That helps with cost, layout, and the exact boundary where bytes cross into code, but the Musi page asks a narrower question: what contract should this field-shaped data example make visible?

{{compare:c99-structs-classes-records}}

## Reading Structs, Classes, and Records from C99

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Records](/learn/book/data/records)
- [Classes](/learn/book/abstractions/classes)
- [Patterns](/learn/book/data/patterns)
