---
title: "Templates and splices"
description: "Build text with interpolation and understand splice syntax near quote forms."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 34
slug: "templates-and-splices"
summary: "Use template literals for interpolated text and splice forms when building syntax."
---

{{snippet:chapter-templates-and-splices}}

## In this chapter

Template literals use backticks and `${...}` interpolation.
They are useful when surrounding text and computed values belong together.
Splice forms such as `#x`, `#(expr)`, and `#[items]` appear in syntax-building contexts where existing values contribute pieces to quoted code.

## Why it matters

String assembly and syntax assembly look similar from far away, but they answer different questions.
Templates produce text-like values.
Splices feed existing values into quoted syntax.
Keeping both forms named helps readers avoid treating every interpolation as a macro.

## Walk through it

Read `` `port ${port}` `` as text with one embedded expression.
Read `#(delta)` inside a quote as a syntax splice, not as string interpolation.
Use templates for user-facing text and splices for code-as-data work.

## Try it next

- Build one template value with a named binding inside it.
- Compare it with a quoted expression that uses `#(...)`.
- Explain which one produces text and which one produces syntax.

## Common mistake

Do not use syntax splices to build ordinary strings. Use templates for text.

## Next

Continue to [Testing](/docs/language/advanced/testing) to return to everyday project workflow.
