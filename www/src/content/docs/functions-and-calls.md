---
title: "Functions and calls"
description: "Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion."
group: "Core language"
section: "Core language"
order: 7
slug: "functions-and-calls"
summary: "Functions, calls, and recursion without extra control syntax."
---

Functions are values you can bind, pass, and call.

## Basic call

{{example:double-function}}

Calls look ordinary: `name(args)`. Function definitions use the same `let` syntax as other bindings.

## Recursion

{{snippet:recursive-case}}

Use `let rec` when a function needs to refer to itself.

## Try it

{{try:functions-and-calls}}

## Next step

Bind and call a function, then add the recursive form and continue to [Data and pattern matching](/docs/data-and-pattern-matching).
