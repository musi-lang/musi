---
title: "Literals"
description: "Meet Musi's everyday literal values before mixing them with operators."
group: "Core syntax"
section: "Core syntax"
order: 6
slug: "literals"
summary: "Start with numbers, strings, booleans, and templates."
---

{{snippet:chapter-literals}}

## What

Literals are values written directly in source: numbers, strings, booleans, and other small values you can read without another definition step.
This example mixes a few literal kinds with nearby derived bindings so you can see what is written directly and what is computed from it.
The language stays readable when you can spot that difference quickly.

## Why

Users rarely ask for "literal theory." They ask how to write a port number, a label, or a comparison flag in working code.
A useful chapter connects literal syntax to ordinary tasks instead of listing token categories.
That lowers early friction before operators, ranges, and structured data show up.

## How

Read `8080` and `"ready"` as direct source values.
Then notice how nearby bindings such as `next`, `same`, and `capped` are built from those literals rather than introducing brand-new syntax categories.
When learning, start with small direct values, then derive one or two computed bindings so you can tell where literal writing stops and expression building begins.

## Try it

- Bind one integer and one string.
- Add one boolean comparison from them or nearby values.
- Rename bindings so result reads like small real code instead of token practice.

## Common mistake

Do not try to learn literals, operators, and data shapes as one giant syntax dump.

## Next

Continue to [Operators](/docs/language/core/operators) to turn those direct values into useful expressions.
