---
title: "Getting started"
description: "Install the tools, know what they do, and start with the right command."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>."
---

Musi has two CLI entry points. Learn that split first and the rest of the toolchain gets simpler.

## Two commands

- `musi` works at package level. Use it for `run`, `check`, `build`, and `test`.
- `music` works on one source graph or built artifact directly.

If you already know tools like `cargo`, `npm`, or `dotnet`, `musi` fills that role. `music` is closer to a direct file runner.

## First setup pass

Start at the [install page](/install), then follow this order:
- install binaries and PATH entries
- create a package
- add first expressions
- run `musi check` and `musi run`

If you want the shortest feedback loop, start with one file and `music`. Move to `musi` once you want package commands and shared project structure.

## Try it

{{try:getting-started}}

## Next step

Open [Install](/install), make one command work, then continue to [First program](/docs/first-program).
