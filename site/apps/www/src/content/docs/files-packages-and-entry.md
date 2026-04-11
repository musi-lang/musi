---
title: "Files, packages, and entry"
description: "Know what <code>musi new</code> creates and what <code>musi run</code> looks for."
group: "Start"
section: "Start"
order: 3
slug: "files-packages-and-entry"
summary: "Packages, <code>musi.json</code>, and the resolved entry file."
---

Packages are the default shape for real projects. A package gives you an entry file and command entrypoints.

## What
You use this when your project grows past one file: dependencies, scripts, and shared entry points stay in one place.

{{snippet:package-commands}}

## Why
`musi` saves you from hand-managing file paths every time you run, test, or build.
- entry resolution follows package config
- commands stay stable across environments
- team members use the same workflow

## How
Keep the generated package shape and use package commands from the root:
- `musi run`
- `musi check`
- `musi build`
- `musi test`

For direct one-off work, use a specific source file or artifact with `music`.

{{snippet:music-direct}}

## When
Use package mode for project workflows and `music` for experimentation, quick checks, or artifact tests.

## Analogy
Think `musi` as the CLI wrapper around your project folder and `music` as a direct file runner.

## Try it
Create a package, confirm which entry file is used, then continue to [Expressions and bindings](/docs/expressions-and-bindings).
