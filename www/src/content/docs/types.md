---
title: "Types and generics"
description: "Read type annotations and generic parameters in the same surface as values."
group: "Types"
section: "Types"
order: 10
slug: "types"
summary: "Type annotations, generic parameters, and direct type application."
---

## What
Types in Musi appear near values and functions.
You can read types without switching to a separate declaration section.

## Why
Type annotations make intent and errors clearer for mixed teams and longer files.

{{snippet:types-basic}}

## How
Add annotations to values and functions, then apply generics where reusable behavior is needed.

{{snippet:types-apply}}

## When
Use explicit typing when APIs are shared across modules or when signatures are not obvious.

## Analogy
Similar to TypeScript annotations, but placed directly in the expression style used throughout Musi.

## Try it
Try the two snippets before moving to [Classes and instances](/docs/classes-instances-and-laws).
