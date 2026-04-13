---
title: "Mutation"
description: "Learn Musi’s explicit mutation surface without mixing it into every lesson."
group: "Start"
section: "Start"
order: 5
slug: "mutation"
summary: "Use mut only when changing a value helps more than rebuilding it."
---

{{snippet:mutable-value}}

## What

Mutation is explicit. You mark a value as mutable, then assign a new value to it.

## Why

Visible mutation makes it easier to tell stable values from changing state.

## How

- Start with `let x := mut 1;`.
- Reassign with `x := 2;`.
- Prefer rebuilding immutable values when that stays clearer.

## Try it

- Create one mutable integer.
- Update it once.
- Compare it with an immutable rewrite.

## Common mistake

Do not add `mut` by reflex.

## Next

Continue to [Literals](/docs/language/core/literals).
