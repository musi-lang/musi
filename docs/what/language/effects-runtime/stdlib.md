---
title: "Stdlib"
description: "Place the standard library on top of foundation and runtime so the layering stays clear."
group: "Effects and runtime"
section: "Effects and runtime"
order: 29
slug: "stdlib"
summary: "Reach for @std modules first in ordinary application code."
---

{{snippet:chapter-stdlib}}

## What

The standard library gives ordinary application code ready-made modules such as `@std/option` and `@std/testing`.
This page pairs a simple import example with a testing import to show stdlib as practical toolbox, not abstract layer diagram.
For most day-to-day code, this is friendliest layer to reach for first.

## Why

After foundation and runtime pages, users need a clear answer to "what do I usually import in normal code?"
That answer is often `@std`.
Putting stdlib in its own chapter prevents the lower layers from looking like default entry point for common tasks.

## How

Read `let Option := import "@std/option";` as explicit acquisition of a higher-level module designed for ordinary code.
Then notice that testing uses same import model: `@std/testing` is still just a module you bind and call through.
When writing app code, start from `@std` modules, then move downward only when you truly need lower-level control.

## Try it

- Import one `@std` module.
- Call one exported helper from it.
- Compare that import with one `musi:*` import and decide which belongs in app code.

## Common mistake

Do not reach for foundation or runtime modules first when a clearer `@std` module already fits the job.

## Next

Continue to [Attributes](/docs/language/advanced/attributes) to finish the learning path with boundary and tooling features.
