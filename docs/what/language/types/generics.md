---
title: "Generics"
description: "Introduce type parameters after annotations and inference make sense."
group: "Types"
section: "Types"
order: 20
slug: "generics"
summary: "Write reusable functions over many types without losing clarity."
---

{{snippet:chapter-generics}}

## What

Generics let one definition work across many concrete types by abstracting over a type parameter such as `T`.
This page keeps the example deliberately small: identity function in generic form, then one explicit application at `Int`.
That is enough to teach the core move without turning the chapter into a full type-system reference.

## Why

Sooner or later users ask how to avoid copy-pasting same function for several types.
Generics answer that need, but they become overwhelming when introduced before annotations and inference are stable.
Here the goal is practical reuse: one function shape, many compatible inputs.

## How

Read `[T]` as parameterization over a type, not over a runtime value.
Then trace where `T` appears in input and output positions to understand what stays same across all uses.
At the call site, `identityFn[Int](port)` makes type application explicit; that can be useful whenever you want the chosen type to be obvious to reader.

## Try it

- Write one generic function with `[T]`.
- Call it once with `Int`.
- Call it again with another obvious type if available.

## Common mistake

Do not introduce several type parameters before one-parameter generic code feels easy to read.

## Next

Continue to [Classes](/docs/language/abstractions/classes) to move from reusable functions to reusable behavior contracts.
