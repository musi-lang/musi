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

## In this chapter

Patterns let code react to value shape instead of only to raw scalar comparisons.
This example defines a small `Port` data type, constructs one value, and then uses `match` to branch on whether a configured value exists.
It is first complete example where data definition, construction, and branching all line up around one real decision.

## Why it matters

Users ask "how do I handle different cases?" as soon as data can vary.
If docs rush into nested destructuring or advanced matching too early, readers get lost before simple constructor matching is stable.
The core win: pattern matching keeps data-dependent branching explicit and readable.

## Walk through it

Read `.Configured(port := 8080)` and `.Default` as two possible shapes of same type.
Then read each `match` arm as answer for one shape, with extracted values such as `port` made available only in arm that matches.
Named payload variants let definitions explain intent directly:

- definition: `| Configured(port : Int, secure : Bool)`
- construction: `.Configured(port := 8080, secure := .True)`
- pattern shorthand: `.Configured(port, secure := enabled)`

Patterns also support guards, aliases, record patterns, array patterns, and alternatives.
Use a guard when shape is not enough, use `as` when the arm needs both whole value and extracted parts, and use alternatives when several shapes share the same result.
When writing your own patterns, start with two or three clear cases and ask what value each branch should produce.
If payload fields already have meaningful names, keep those names visible in the pattern instead of falling back to anonymous tuple-like slots.

## Try it next

- Define one data type with two cases.
- Construct one variant value.
- Write `match` expression that returns different result for each shape.

## Common mistake

Do not begin with deeply nested pattern trees before constructor-level branching feels easy.
Also do not mix positional and named payload style inside one variant definition.

## Next

Continue to [Files](/docs/language/organization/files) to see how these language forms live inside ordinary source files.
