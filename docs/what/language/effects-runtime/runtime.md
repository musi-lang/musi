---
title: "Runtime"
description: "Learn what runtime-backed imports are for and why they are separate from stdlib helpers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 28
slug: "runtime"
summary: "Use musi:runtime for runtime-backed capabilities and host services."
---

{{snippet:runtime-import}}

## What

`musi:runtime` is the boundary for runtime-backed services.

## Why

Host-powered behavior has different boundaries than pure helpers.

## How

- Read `musi:runtime` imports as lower-level capability surfaces.
- Prefer `@std` wrappers in ordinary code.
- Reach lower only when you are building boundaries.

## Try it

- Import `musi:runtime`.
- Call one helper.
- Compare it with `@std/env`.

## Common mistake

Do not confuse `musi:runtime` with general-purpose stdlib.

## Next

Continue to [Stdlib](/docs/language/effects-runtime/stdlib).
