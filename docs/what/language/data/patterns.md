---
title: "Patterns"
description: "Learn pattern matching after records and arrays, not before."
group: "Data"
section: "Data"
order: 14
slug: "patterns"
summary: "Use case and destructuring to branch on data shape."
---

{{snippet:case-port}}

## What

Patterns inspect structured values by shape.

## Why

Pattern matching lands much better once you already understand the shapes you are matching.

## How

- Define a small `data` type.
- Construct one value.
- Match it with `case ... of`.

## Try it

- Define a two-case `data` type.
- Construct each case once.
- Return different values from a `case`.

## Common mistake

Do not start with deeply nested patterns while simple constructor matching still feels new.

## Next

Continue to [Files](/docs/language/organization/files).
