---
title: "Musi language book"
description: "Learn Musi through a chaptered guide that starts with simple expressions and climbs toward data, types, effects, and tooling."
group: "Language"
section: "Language"
order: 0
slug: "language"
summary: "A consumer-facing Musi guide that starts small, explains syntax, and keeps building toward real programs."
---

Musi is easiest to learn as a guided climb, not as a grammar dump.
The chapters in this book start with plain values and `let`, then keep widening the picture: blocks, functions, data, files, types, abstractions, effects, foreign boundaries, and tooling.

## How to read this book

Read it like a language guide, not a reference.
Most chapters center on one compilable code example, then explain how to read it, when to use the construct, and what Musi does differently from many C-like languages.

If you already write C, Rust, Java, JavaScript, TypeScript, or Python, keep these translation rules in mind from the start:

- Musi is expression-first. Blocks produce values.
- There is no `return` keyword for ordinary function flow. The last expression is the result.
- There are no loop statements. Repetition usually comes from recursion, ranges, higher-order helpers, or effect-driven iteration.
- Mutation is explicit with `mut`.
- Algebraic data and pattern matching are ordinary language features, not add-ons.

## What you will learn

The path is a spiral staircase.
Early chapters teach just enough syntax to read and run code.
Later chapters revisit the same ideas with more power:

- `let`, blocks, and expression flow
- functions, calls, methods, pipelines, and operators
- arrays, records, variants, and patterns
- imports, files, and packages
- type annotations, inference, generics, and constraints
- classes, instances, laws, effects, handlers, and runtime boundaries
- attributes, FFI, testing, and tool workflow

## What Musi does not have

Musi does not try to look like every mainstream language.
That means some expected features are intentionally replaced by simpler rules:

- No statement-only function body model. A function body is an expression.
- No `return` keyword as normal control flow.
- No `for`, `while`, `break`, or `continue` statements.
- No class-based object model. Reuse comes from functions, methods, data, classes, and instances.
- No hidden ambient mutation. If state changes, `mut` and assignment make it visible.

Those choices show up gradually through the chapters instead of all at once.

## Start here

If Musi is new, read the parts in order:

1. Start
2. Core syntax
3. Data
4. Organization
5. Types
6. Abstractions
7. Effects and runtime
8. Advanced and tooling
