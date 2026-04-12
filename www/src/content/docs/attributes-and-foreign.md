---
title: "Attributes and foreign declarations"
description: "Use public attributes and foreign declarations."
group: "Abstractions"
section: "Abstractions"
order: 13
slug: "attributes-and-foreign"
summary: "Public attributes, foreign bindings, and symbol metadata."
---

## What
Attributes and foreign declarations are explicit annotations on declarations.

## When
Use foreign declarations when you integrate existing runtime boundaries.

## Why
They connect Musi declarations to external systems and symbol semantics without hiding behavior in implicit magic.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Use `@link`, `@when`, `@repr`, and `@layout` where external binding or metadata is required.

{{snippet:attr-link-foreign}}

{{snippet:foreign-puts}}

## Analogy
Like adding attributes in C# or annotations in Java, but in a compact declaration style.

## Try it
Review attributes first, then the foreign example, then continue to [Quote and syntax values](/docs/quote-and-syntax).
