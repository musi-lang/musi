---
title: "Ranges"
description: "Learn Musi’s range operators in isolation before they appear inside larger code."
group: "Core syntax"
section: "Core syntax"
order: 8
slug: "ranges"
summary: "Read open, closed, and spread-like range forms without guessing."
---

{{snippet:ranges-basic}}

## What

Ranges describe ordered spans such as `0..10` or `0..<10`.

## Why

Dot-heavy syntax is easy to misread unless you learn range forms separately from spread forms.

## How

- Read `..` as a range form.
- Read `..<` as a boundary-sensitive form.
- Keep `...` mentally separate.

## Try it

- Write one `0..10`.
- Write one `0..<10`.
- Explain the difference to yourself.

## Common mistake

Do not assume every dotted form is a range.

## Next

Continue to [Functions](/docs/language/core/functions).
