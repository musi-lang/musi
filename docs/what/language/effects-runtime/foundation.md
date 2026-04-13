---
title: "Foundation"
description: "Separate language foundation from runtime and stdlib layers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 27
slug: "foundation"
summary: "Understand what belongs to musi:core before reaching for stdlib modules."
---

{{snippet:foundation-import}}

## What

`musi:core` holds foundation-level language concepts.

## Why

Beginners need a clear answer to “what is built in” versus “what is library code”.

## How

- Treat `musi:core` as low-level foundation.
- Reach for it rarely in ordinary app code.
- Prefer `@std` for day-to-day helpers.

## Try it

- Import one foundation module.
- Name why it feels lower-level.
- Compare it with `@std`.

## Common mistake

Do not build ordinary app APIs directly on foundation modules when a clearer stdlib layer exists.

## Next

Continue to [Runtime](/docs/language/effects-runtime/runtime).
