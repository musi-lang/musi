---
title: "Imports and packages"
description: "Import modules and use the main namespace families."
group: "Core language"
section: "Core language"
order: 4
slug: "imports-and-packages"
summary: "Import expressions, <code>@std</code>, and the <code>musi:*</code> foundation namespace."
---

## What
Imports let you pull shared code into the current file: standard features from `@std` and lower-level building blocks from `musi:*` when needed.

## When
Use imports in most new files.
- Start with `@std` for everyday tasks.
- Use `musi:*` for boundary-level interoperability and foundation-level work.

## Why
This keeps code short and reusable while avoiding copy-paste or duplicated helpers.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Add imports at the top of your file, then use imported names directly in regular expressions.

## Compare
{{example:import-stdlib}}

## Analogy
Like Python imports or TypeScript module imports: one place for shared functionality, one place for lower-level tools.

## Try it
Replace duplicated snippets with imported names, then continue to [Expressions and bindings](/docs/expressions-and-bindings).
