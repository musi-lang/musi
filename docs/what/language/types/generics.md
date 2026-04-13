---
title: "Generics"
description: "Introduce type parameters after annotations and inference make sense."
group: "Types"
section: "Types"
order: 20
slug: "generics"
summary: "Write reusable functions over many types without losing clarity."
---

{{snippet:types-basic}}

{{snippet:types-apply}}

## What

Generics let one definition work across many types by using type parameters such as `T`.

## Why

Without generics, reusable code quickly turns into copy-paste or overly concrete helpers.

## How

- Add a type parameter list like `[T]`.
- Use that parameter in inputs and outputs.
- Apply the generic explicitly when it helps reading.

## Try it

- Write one generic identity function.
- Call it with `Int`.
- Call it with another type.

## Common mistake

Do not add several type parameters before one-parameter generic code feels natural.

## Next

Continue to [Classes](/docs/language/abstractions/classes).
