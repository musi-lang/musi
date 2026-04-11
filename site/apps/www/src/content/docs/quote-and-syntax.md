---
title: "Quote and syntax values"
description: "Work with quoted syntax and embedded expressions."
group: "Abstractions"
section: "Abstractions"
order: 13
slug: "quote-and-syntax"
summary: "Quoted expressions, quoted blocks, and splice forms."
---

## What
Quote syntax lets you treat code as data and then selectively run it inside templates or macros.

## Why
This is useful for metaprogramming, AST transforms, and delayed execution patterns.

{{snippet:quote-expr}}

## How
Use quote for expressions, then splice as needed with the quoted splice forms.

{{snippet:quote-block}}

## When
Use it when code shape matters at runtime or when you want reusable template-style fragments.

## Analogy
Think of it like template literals plus parser-level code as data in one step.

## Try it
Try both quote snippets, then continue to [Foundation and standard library](/docs/foundation-and-standard-library).
