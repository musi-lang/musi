---
title: "Imports and exports"
description: "Use imports and exports after package shape is clear."
group: "Code organization"
section: "Code organization"
order: 17
slug: "imports-and-exports"
summary: "Bring code in explicitly and expose only what other files need."
---

{{example:import-stdlib}}

{{snippet:export-import}}

## What

Imports bring values or modules into scope. Exports mark names other files may use.

## Why

Explicit module boundaries make dependency flow visible.

## How

- Import with `let Name := import "spec";`.
- Export only names you want to publish.
- Prefer small public surfaces.

## Try it

- Import one `@std` module.
- Export one helper.
- Use it from a second file.

## Common mistake

Do not use imports as a substitute for naming discipline.

## Next

Continue to [Type annotations](/docs/language/types/type-annotations).
