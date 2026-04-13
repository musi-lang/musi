---
title: "Type annotations"
description: "Introduce type annotations before inference and generics."
group: "Types"
section: "Types"
order: 18
slug: "type-annotations"
summary: "Add type information where it helps readers and tools."
---

{{snippet:types-basic}}

## What

Type annotations tell readers and the compiler what kind of value or result you expect.

## Why

They provide anchors before inference and generic code enter the picture.

## How

- Annotate important bindings first.
- Annotate public helpers before tiny locals.
- Use annotations for clarity, not decoration.

## Try it

- Annotate one integer binding.
- Annotate one function result.
- Compare readability.

## Common mistake

Do not annotate everything by habit.

## Next

Continue to [Type inference](/docs/language/types/type-inference).
