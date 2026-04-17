---
title: "Musi for Developers"
description: "Map common habits from other languages into Musi code."
group: "Musi for Developers"
section: "Musi for Developers"
order: 9
slug: "developers"
summary: "Translate familiar habits into Musi without carrying over syntax that does not fit."
---

This part is for readers who already write software in another language.
Each guide starts from habits you already have, then shows how Musi writes the same idea with expressions, `let`, data constructors, pattern matching, effects, packages, and explicit unsafe boundaries.

Rust and JavaScript/TypeScript get deeper comparisons because their common habits hide different questions.
Rust needs a slower translation for value-based mutation and type-driven boundaries.
JavaScript and TypeScript need a slower translation for objects, nullable values, promises, discriminated unions, modules, and TypeScript 5.9 type habits.

## Choose Your Starting Point

Pick the language you use most often.
The guide will not teach that language back to you; it names the habits that usually transfer well and the ones that need a different Musi shape.

## Shared Musi Model

Musi code reads as values flowing through expressions.
Blocks produce values, `match` chooses by shape, records keep named fields, and `request` asks an effect handler for work.
There is no `return` keyword in ordinary function bodies.
The final expression is the value.

## Guides

- [Musi for JavaScript and TypeScript Developers](/learn/book/developers/guides/javascript-typescript)
- [Musi for Rust Developers](/learn/book/developers/guides/rust)
