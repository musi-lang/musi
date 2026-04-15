---
title: "Dependent Types"
description: "Use values in type positions when shape, size, or protocol state should be visible in the type."
group: "Types"
section: "Types"
order: 23
slug: "dependent-types"
summary: "Use value-indexed types, indexed data results, `partial`, and `~=` without turning Musi into a proof assistant."
---

Musi supports dependent-style surface forms where type parameters can mention values, such as a length in a vector type. `partial` marks definitions that may not be total at compile time.

{{snippet:chapter-dependent-types}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Classes](/learn/book/abstractions/classes-instances-laws/classes).
