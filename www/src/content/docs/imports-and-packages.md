---
title: "Imports and packages"
description: "Import modules and use the main namespace families."
group: "Core language"
section: "Core language"
order: 4
slug: "imports-and-packages"
summary: "Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace."
---

Imports are expressions. Bind them with `let`, then use the imported value like any other name.

## Default rule

- Start with `@std`.
- Reach for `musi:*` only when you need lower-level foundation modules.

That keeps application code on the standard library path and keeps compiler-facing pieces explicit.

## Example

{{example:import-stdlib}}

Imports can appear anywhere an expression can appear, but keeping them near the top of a file is still the easiest style to read.

## Try it

{{try:imports-and-packages}}

## Next step

Replace duplicated snippets with imported names, then continue to [Expressions and bindings](/docs/expressions-and-bindings).
