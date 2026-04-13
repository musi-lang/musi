---
title: "Effects"
description: "Introduce effect vocabulary before using clauses or handlers."
group: "Effects and runtime"
section: "Effects and runtime"
order: 24
slug: "effects"
summary: "Understand effects as requests for work, not immediate hidden side effects."
---

{{snippet:effect-console}}

{{snippet:perform-console}}

## What

An effect describes operations that code may request.

## Why

Visible effect requests are easier to reason about than hidden ambient side effects.

## How

- Define a small effect.
- Perform one operation from it.
- Read the operation as a request, not magic global access.

## Try it

- Define one effect.
- Perform one operation.
- Describe what must now handle it.

## Common mistake

Do not try to learn full handlers before the request model is stable.

## Next

Continue to [Using](/docs/language/effects-runtime/using).
