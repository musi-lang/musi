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

Rust gets the deepest comparison because the mutability model and type-driven habits need a slower translation.
That guide goes further into `let mut x`, value-based mutation, and the way Musi keeps the mutable part visible instead of scattered through the whole binding model.

## Choose Your Starting Point

Pick the language you use most often.
The guide will not teach that language back to you; it names the habits that usually transfer well and the ones that need a different Musi shape.

## Shared Musi Model

Musi code reads as values flowing through expressions.
Blocks produce values, `match` chooses by shape, records keep named fields, and `request` asks an effect handler for work.
There is no `return` keyword in ordinary function bodies.
The final expression is the value.

## Guides

- [Musi for C Developers](/learn/book/developers/guides/c)
- [Musi for C# Developers](/learn/book/developers/guides/csharp)
- [Musi for C++ Developers](/learn/book/developers/guides/cpp)
- [Musi for Go Developers](/learn/book/developers/guides/go)
- [Musi for Java Developers](/learn/book/developers/guides/java)
- [Musi for JavaScript Developers](/learn/book/developers/guides/javascript)
- [Musi for Python Developers](/learn/book/developers/guides/python)
- [Musi for Rust Developers](/learn/book/developers/guides/rust)
- [Musi for TypeScript Developers](/learn/book/developers/guides/typescript)
