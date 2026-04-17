---
title: "Comptime"
description: "Run selected Musi expressions during compilation and use the result as a first-class value."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 33
slug: "comptime"
summary: "Use value-position comptime for constants, specialization, and generated syntax."
---

{{snippet:chapter-comptime}}

`comptime` is a value-position prefix, like `mut` in parameter position.
Read `comptime add(20, 22)` as "evaluate this expression while compiling, then use the result in the program."
Do not write `comptime(expr)`: `comptime` is not a function call.

## Specialization

A parameter can be marked `comptime` when the callable needs that value during compilation.
Calls to that function specialize by the compile-time argument value.

{{snippet:comptime-parameter}}

## Values

Compile-time evaluation can produce ordinary values such as integers, strings, data variants, arrays, closures, and syntax.
Type, module, foreign, effect, and class handles can also move through the compile-time value system.
Continuations cannot escape compile-time evaluation.

## Generated Syntax

`quote` produces `Syntax`.
`comptime quote { ... }` expands quoted module items before the surrounding module finishes checking.
Named `Syntax` values can be expanded with `comptime name;`.

{{snippet:comptime-quote-module}}

## Effects

Handled effects can run inside `comptime`.
Unhandled host effects need the effect operation marked `@comptimeSafe`, and the compiler session must provide a compile-time host for that operation.
Direct foreign calls are rejected during compile-time evaluation.

{{snippet:comptime-safe-effect}}

## Mistake to Avoid

Do not use `comptime` to hide runtime side effects.
Use it for values the compiler must know, generated syntax, and small deterministic setup.

## Next Page

Continue to [Templates and splices](/learn/book/advanced/templates-and-splices) to separate text interpolation from syntax splicing.
