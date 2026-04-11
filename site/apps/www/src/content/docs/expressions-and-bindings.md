---
title: "Expressions and bindings"
description: "Read Musi through <code>let</code>, sequences, and <code>case</code>."
group: "Core language"
section: "Core language"
order: 4
slug: "expressions-and-bindings"
summary: "The base reading model for names, sequences, and branching."
---

## What
Expressions and bindings are the foundation of Musi reading.
Start with a bound name, then read later expressions from the top of the file down.

{{snippet:let-binding}}

## Why
This model is predictable for Python/JS users: each line can introduce data, then later lines consume it.

## How
Use `let` for names and `;` for expression boundaries.

{{snippet:sequence}}

Case expressions are the branching form in this surface.

{{snippet:case-port}}

## When
Use these patterns when you want readable scripts, data preparation, and deterministic command flow.

## Analogy
Like a recipe card: bind each ingredient first, then assemble a final step.

## Try it
Read the two snippets, then move to [Functions and calls](/docs/functions-and-calls).
