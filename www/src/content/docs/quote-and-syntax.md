---
title: "Quote and syntax values"
description: "Use quote and splice for reusable code templates and syntax-driven workflows."
group: "Abstractions"
section: "Abstractions"
order: 14
slug: "quote-and-syntax"
summary: "Quoted expressions, splice forms, and practical metaprogramming patterns."
---

## What
Quote syntax lets you treat code as data, then splice values or sub-expressions into that code shape.

## When
Use it when code shape itself is the reusable unit:
- generating repeated expression forms,
- assembling small syntax templates,
- or transforming syntax before execution.

## Why
Without quote, you usually duplicate near-identical function bodies.

{{snippet:quote-without-meta}}

With quote and splice, you keep one template shape and vary only the changing parts.

{{snippet:quote-with-meta}}

This is the basis for metaprogramming, AST transforms, and delayed execution patterns.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Start with a quoted expression or block, then splice dynamic parts with the quoted splice forms.

{{snippet:quote-expr}}

{{snippet:quote-block}}

## Compare
{{example:quote-metaprogramming}}

## Analogy
Think of it like template literals plus AST builders in one surface.

## Try it
Take one duplicated helper pair, replace it with one quoted template, then continue to [Foundation and standard library](/docs/foundation-and-standard-library).
