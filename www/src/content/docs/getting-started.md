---
title: "Getting started"
description: "Install the tools, know what they do, and start with the right command."
group: "Start"
section: "Start"
order: 1
slug: "getting-started"
summary: "Install, PATH setup, and the difference between <code>musi</code> and <code>music</code>."
---

This guide is for people who want to write Musi, run packages, and get useful behavior quickly.

## What
You install the command-line tools once, then use Musi as your package and source language for small scripts, services, or experiments.

## Why
Musi separates package workflows from direct file workflows.
- `musi` handles package config, entry resolution, and project scripts.
- `music` runs one source graph or artifact directly.

## How
Start at the [install page](/install), then follow this order:
- install binaries and PATH entries
- create a package
- add first expressions
- run `musi check` and `musi run`

## When
Use this sequence when you have a new machine, a new clone, or you are onboarding from Python, JS, or TS and want the smallest working first step.

## Analogy
Treat `musi` like `npm` or `cargo` for project lifecycle, and `music` like a direct `node`/`deno`-style file runner.

## Try it
Open install notes on [Install](/install), then continue to [First program](/docs/first-program).
