---
title: "Quote and syntax values"
description: "Use quote and splice for reusable code templates and syntax-driven workflows."
group: "Abstractions"
section: "Abstractions"
order: 14
slug: "quote-and-syntax"
summary: "Quoted expressions, splice forms, and practical metaprogramming patterns."
---

Quote syntax lets you treat code as data, then splice values or sub-expressions into that code shape.

## Without quote

{{snippet:quote-without-meta}}

## With quote and splice

{{snippet:quote-with-meta}}

Use `quote` when code shape itself is data you want to build or transform.

## Small forms

{{snippet:quote-expr}}

{{snippet:quote-block}}

## Compare

{{example:quote-metaprogramming}}

Splice forms such as `#name` and `#(expr)` are only valid inside quote contexts.

## Try it

{{try:quote-and-syntax}}

## Next step

Take one duplicated helper pair, replace it with one quoted template, then continue to [Foundation and standard library](/docs/foundation-and-standard-library).
