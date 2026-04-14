---
title: "Packages"
description: "Learn package roots and entry files after single-file work makes sense."
group: "Code organization"
section: "Code organization"
order: 16
slug: "packages"
summary: "Move from one file to package-managed code without changing mental models."
---

{{snippet:chapter-packages}}

## What

A package groups source files, manifest data, and package-level commands under one project root.
The `musi new hello` flow shows what package work looks like when you are no longer just checking one scratch file.
This is about project shape, not new language semantics.

## Why

Users need a clear moment where they switch from "I am learning syntax in one file" to "I am building a project I will rerun, test, and grow."
Without that transition, `musi run`, `musi test`, and manifest concepts feel arbitrary.
A practical package example gives those commands a home.

## How

Read the command sequence as package lifecycle: create project, move into root, then run package entry point.
After that, inspect generated structure and connect it back to earlier file model: package entry is still just Musi source, now managed by project tooling.
Use package workflow when code needs multiple files, dependency tracking, or repeatable commands.

## Try it

- Create one package with `musi new`.
- Open generated entry file.
- Run `musi run` from package root.

## Common mistake

Do not assume package workflow makes direct `music` file work obsolete.

## Next

Continue to [Imports and exports](/docs/language/organization/imports-and-exports) to connect files without turning everything public.
