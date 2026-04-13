---
title: "Arrays and slices"
description: "Read sequence-shaped data without mixing it into pattern syntax yet."
group: "Data"
section: "Data"
order: 13
slug: "arrays-and-slices"
summary: "Store ordered values and learn where slices fit."
---

{{snippet:record-array}}

{{snippet:slice-helpers}}

## What

Arrays hold ordered items. Slices describe sequence-oriented helper work and views.

## Why

Sequences are easier to learn when they are not mixed with records and patterns at the same time.

## How

- Build arrays with `[a, b, c]`.
- Use spread to build related arrays.
- Reach for `@std/slice` helpers when you need sequence operations.

## Try it

- Create one array.
- Build a second array with spread.
- Call one `Slice` helper.

## Common mistake

Do not assume arrays and slices are the same word for the same thing.

## Next

Continue to [Patterns](/docs/language/data/patterns).
