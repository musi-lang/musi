---
title: "Templates and Splices"
description: "Build text with interpolation and understand splice syntax near quote forms."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 34
slug: "templates-and-splices"
summary: "Use template literals for interpolated text and splice forms when building syntax."
---
Templates and splices help generate repeated code from a smaller pattern. They are useful when many declarations share the same structure but differ in a few names or types.

{{snippet:chapter-templates-and-splices}}

Think of a template like a form letter. It saves time only if the shared shape is real. If every filled-in copy needs special exceptions, write the declarations directly.

## Repetition versus regularity

Repeated code is not always a problem. Regular repeated code can be generated safely. Irregular repeated code is often clearer when written out.

## Review the result

Generated declarations should be easy to inspect. If the splice makes formatter output, diagnostics, or semantic highlighting confusing, the template is doing too much.

Templates work best for regular families: one operation per field, one wrapper per foreign symbol, or one test case per known example. They work poorly when each generated piece immediately needs hand-written exceptions. In that case the repetition is telling you about missing design, not missing automation.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
