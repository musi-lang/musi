---
title: "Templates and Splices"
description: "Build text with interpolation and understand splice syntax near quote forms."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 34
slug: "templates-and-splices"
summary: "Use template literals for interpolated text and splice forms when building syntax."
---

{{snippet:chapter-templates-and-splices}}

## Boundary Tool

Template literals use backticks and `${...}` interpolation.
They are useful when surrounding text and computed values belong together.
Splice forms such as `#x`, `#(expr)`, and `#[items]` appear in syntax-building contexts where existing values contribute pieces to quoted code.

## When to Reach for It

String assembly and syntax assembly look similar from far away, but they answer different questions.
Templates produce text-like values.
Splices feed existing values into quoted syntax.
Keeping both forms named helps readers avoid treating every interpolation as a macro.

## Read the Boundary

Read `` `port ${port}` `` as text with one embedded expression.
Read `#(delta)` inside a quote as a syntax splice, not as string interpolation.
Use templates for user-facing text and splices for code-as-data work.

## Small Exercise

- Build one template value with a named binding inside it.
- Compare it with a quoted expression that uses `#(...)`.
- Explain which one produces text and which one produces syntax.

## Mistake to Avoid

Do not use syntax splices to build ordinary strings. Use templates for text.

## Next Page

Continue to [Testing](/learn/book/advanced/testing) to return to everyday project workflow.
