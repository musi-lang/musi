---
title: "Functions and calls"
description: "Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion."
group: "Core language"
section: "Core language"
order: 7
slug: "functions-and-calls"
summary: "Functions, calls, and recursion without extra control syntax."
---

## What
Functions are values you can bind, pass, and call.

## When
Use recursion for traversals, accumulations, and simple parsers when a loop is not needed.

## Why
This gives a direct way to organize repeated logic without relying on special syntax blocks.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Call named functions like usual value calls, then define recursion in the same surface with `let rec`.

## Compare
{{example:double-function}}

{{snippet:recursive-case}}

## Analogy
Like defining a helper in Python and calling itself from its own body, but in one value-binding style.

## Try it
Bind and call a function, then add the recursive form and continue to [Data and pattern matching](/docs/data-and-pattern-matching).
