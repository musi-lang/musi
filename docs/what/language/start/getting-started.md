---
title: "Getting started"
description: "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install tools, install Musi, and learn the two command lanes."
---

{{snippet:chapter-getting-started}}

## What

Getting started in Musi means setting up one toolchain, then learning that there are two command lanes.
Use `music` when you are working directly with one file. Use `musi` when you are working inside a package.
That split matters early because it tells you whether you are experimenting with syntax or managing a project.

## Why

Many "how do I run this?" questions come from not knowing which command lane owns which job.
If the install page only says "install Musi," beginners still do not know whether to reach for `music check index.ms` or `musi run`.
This page should make the mental model concrete: one binary for direct file work, one binary for package workflow.

## How

Read the first command block as fastest path to a working install, then keep the Cargo path as fallback when you want a local clone or explicit build.
After installation, verify `music` first with a single-file check command.
Then create one package and run `musi` inside it so the two lanes become separate muscle memory instead of one blurred command surface.

## Try it

- Install Musi with one command path.
- Run `music check index.ms` on a scratch file.
- Create one package with `musi new hello` and run it.

## Common mistake

Do not treat `music` and `musi` as duplicate names for same workflow.

## Next

Continue to [First program](/docs/language/start/first-program) to use the direct file lane on the smallest possible Musi file.
