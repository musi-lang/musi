---
title: "Files, packages, and entry"
description: "Know what <code>musi new</code> creates and what <code>musi run</code> looks for."
group: "Start"
section: "Start"
order: 3
slug: "files-packages-and-entry"
summary: "Packages, <code>musi.json</code>, and the resolved entry file."
---

Use plain files while you experiment. Use a package once you want repeatable commands and a stable project root.

## Package workflow

{{snippet:package-commands}}

- `musi` reads package config, resolves the entry file, and runs project commands.
- `music` stays useful for direct checks on one source file or built artifact.

## Why packages help

Packages remove repeated path handling. They also give everyone on a project the same command surface.

## Direct mode

{{snippet:music-direct}}

Use direct mode for one-off experiments, parser checks, and small examples that do not need package metadata yet.

## Try it

{{try:files-packages-and-entry}}

## Next step

Create a package, confirm which entry file is used, then continue to [Imports and packages](/docs/imports-and-packages).
