---
title: "Stdlib"
description: "Place the standard library on top of foundation and runtime so the layering stays clear."
group: "Effects and runtime"
section: "Effects and runtime"
order: 29
slug: "stdlib"
summary: "Reach for @std modules first in ordinary application code."
---

{{example:import-stdlib}}

{{snippet:stdlib-testing-import}}

## What

`@std` is the user-facing standard library.

## Why

Most users should learn one clear library surface first.

## How

- Import focused stdlib packages directly.
- Prefer modules like `@std/option` and `@std/testing`.
- Use the root catalog only when it reads better.

## Try it

- Import one focused `@std` module.
- Call one helper.
- Add one test helper import.

## Common mistake

Do not reach for low-level `musi:*` imports when a clear `@std` module already solves the job.

## Next

Continue to [Attributes](/docs/language/advanced/attributes).
