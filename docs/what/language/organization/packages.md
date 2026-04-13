---
title: "Packages"
description: "Learn package roots and entry files after single-file work makes sense."
group: "Code organization"
section: "Code organization"
order: 16
slug: "packages"
summary: "Move from one file to package-managed code without changing mental models."
---

{{snippet:quickstart}}

## What

A package groups files, dependencies, and entry settings under `musi.json`.

## Why

Package structure matters once code grows, but it is too much ceremony for a first file.

## How

- Create a package with `musi new hello`.
- Inspect the generated structure.
- Use `musi run`, `musi check`, and `musi test`.

## Try it

- Create one package.
- Open its entry file.
- Run `musi run`.

## Common mistake

Do not assume package work replaces direct file work.

## Next

Continue to [Imports and exports](/docs/language/organization/imports-and-exports).
