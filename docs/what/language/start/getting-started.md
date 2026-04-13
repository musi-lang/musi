---
title: "Getting started"
description: "Install prerequisites, install Musi with script or Cargo, and learn when to use music versus musi."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install tools, install Musi, and learn the two command lanes."
---

{{snippet:install-curl}}

{{snippet:install-cargo}}

## What

Musi currently installs through Cargo. You can use the install script or install from a local clone.

## Why

Beginners need one short setup story before they learn the command lanes.

## How

- Install Rust and libffi first.
- Run one install command.
- Ensure Cargo's bin directory is on `PATH`.
- Use `music` for direct file work.
- Use `musi` for package work.

## Try it

- Run one install command.
- Run one `music check index.ms`.
- Run one `musi new hello`.

## Common mistake

Do not treat `music` and `musi` as duplicates.

## Next

Continue to [First program](/docs/language/start/first-program).
