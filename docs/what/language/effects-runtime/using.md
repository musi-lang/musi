---
title: "Using"
description: "Learn using syntax before full handlers so capability requirements stay visible."
group: "Effects and runtime"
section: "Effects and runtime"
order: 25
slug: "using"
summary: "Read and write using clauses as explicit capability flow."
---

{{snippet:using-signature}}

## What

`using` spells out effect requirements in declarations and types.

## Why

Capability flow stays readable only when requirements stay explicit.

## How

- Read `using { Console }` as a required capability set.
- Put `using` where the signature should expose that need.
- Keep early examples to one capability.

## Try it

- Write one signature with `using`.
- Read it aloud in plain English.
- Compare it with a hidden requirement.

## Common mistake

Do not treat `using` as decoration.

## Next

Continue to [Handlers](/docs/language/effects-runtime/handlers).
