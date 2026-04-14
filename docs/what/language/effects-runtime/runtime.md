---
title: "Runtime"
description: "Learn what runtime-backed imports are for and why they are separate from stdlib helpers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 28
slug: "runtime"
summary: "Use musi:runtime for runtime-backed capabilities and host services."
---

{{snippet:chapter-runtime}}

## What

`musi:runtime` exposes runtime-backed capabilities tied to the host environment.
The example uses `envGet("HOME")`, which is good because it looks like something ordinary code might want while still clearly depending on runtime presence.
This is where Musi crosses from pure language surface into host-connected services.

## Why

Users need to know why runtime imports are separate from both foundation and `@std` modules.
If docs flatten those layers together, it becomes hard to tell what is portable language code and what depends on runtime support.
A concrete runtime import makes that boundary easier to reason about.

## How

Read `import "musi:runtime"` as explicit opt-in to host-backed functionality.
Then read `Runtime.envGet("HOME")` as normal call over imported module, with extra understanding that result depends on surrounding runtime environment.
Use runtime modules when code truly needs host services, and keep that dependency visible instead of smuggling it in through unrelated helpers.

## Try it

- Import `musi:runtime`.
- Call one runtime-backed function.
- Explain what part of result depends on host environment.

## Common mistake

Do not present runtime-backed imports as interchangeable with pure stdlib helpers.

## Next

Continue to [Stdlib](/docs/language/effects-runtime/stdlib) to see what ordinary application code usually reaches for first.
