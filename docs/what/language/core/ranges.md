---
title: "Ranges"
description: "Learn Musi's range operators in isolation before they appear inside larger code."
group: "Core syntax"
section: "Core syntax"
order: 8
slug: "ranges"
summary: "Read open, closed, and spread-like range forms without guessing."
---

{{snippet:chapter-ranges}}

## What

Ranges are compact value forms for spans such as "zero through ten" or "zero up to but not including ten."
Musi makes the endpoint choice visible in the operator itself, which is why this topic deserves its own page instead of being buried inside larger examples.
The syntax is small, but the meaning matters.

## Why

Readers often trip on off-by-one errors long before they trip on advanced language features.
If docs rush past range syntax, users still have to ask whether end value is included, excluded, or context dependent.
This chapter should make those decisions visible early so later APIs that consume ranges feel predictable.

## How

Read `0..10` as closed range and `0..<10` as half-open range.
The example ends with `closed;` so you can see range syntax is still ordinary expression-producing code.
When choosing between forms, decide first whether end value belongs inside result, then pick operator that states that choice directly.

## Try it

- Create one closed range.
- Create one half-open range with same endpoints.
- Write down which values differ between them.

## Common mistake

Do not assume every range form means same endpoint behavior with different punctuation.

## Next

Continue to [Functions](/docs/language/core/functions) to package repeated expression logic behind a reusable name.
