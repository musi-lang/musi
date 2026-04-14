---
title: "Imports and exports"
description: "Use imports and exports after package shape is clear."
group: "Code organization"
section: "Code organization"
order: 17
slug: "imports-and-exports"
summary: "Bring code in explicitly and expose only what other files need."
---

{{snippet:chapter-imports-and-exports}}

## What

Imports bring code or modules into scope, and exports decide which names other files may use.
This page pairs a standard-library import with a local export/import cycle so the boundary is visible from both directions.
That makes module flow concrete instead of abstract.

## Why

As soon as code crosses file boundaries, users ask two questions: "how do I bring this in?" and "how do I expose that out?"
If docs only answer one of them, people still end up guessing about module ownership.
Clear import/export examples help keep dependencies explicit and public surfaces small.

## How

Read `let Option := import "@std/option";` as binding imported module to a local name you can call through.
Then read `export let answer := 42;` as deliberate publication of one binding, not automatic exposure of whole file.
When organizing code, import only what you need, export only what other files truly depend on, and keep local helpers unexported by default.

## Try it

- Import one `@std` module into a file.
- Export one helper from another file.
- Use exported name through local import.

## Common mistake

Do not use imports and exports as substitute for deciding which names should stay local.

## Next

Continue to [Type annotations](/docs/language/types/type-annotations) to make important value shapes explicit once code spans more than one toy file.
