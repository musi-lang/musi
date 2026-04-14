---
title: "Patterns"
description: "Learn pattern matching after records and arrays, not before."
group: "Data"
section: "Data"
order: 14
slug: "patterns"
summary: "Use match and destructuring to branch on data shape."
---

{{snippet:chapter-patterns}}

## What

Patterns let code react to value shape instead of only to raw scalar comparisons.
This example defines a small `Port` data type, constructs one value, and then uses `match` to branch on whether a configured value exists.
It is first complete example where data definition, construction, and branching all line up around one real decision.

## Why

Users ask "how do I handle different cases?" as soon as data can vary.
If docs rush into nested destructuring or advanced matching too early, readers get lost before simple constructor matching is stable.
This page should teach the core win: pattern matching keeps data-dependent branching explicit and readable.

## How

Read `.Configured(8080)` and `.Default` as two possible shapes of same type.
Then read each `match` arm as answer for one shape, with extracted values such as `port` made available only in arm that matches.
When writing your own patterns, start with two or three clear cases and ask what value each branch should produce.

## Try it

- Define one data type with two cases.
- Construct one variant value.
- Write `match` expression that returns different result for each shape.

## Common mistake

Do not begin with deeply nested pattern trees before constructor-level branching feels easy.

## Next

Continue to [Files](/docs/language/organization/files) to see how these language forms live inside ordinary source files.
