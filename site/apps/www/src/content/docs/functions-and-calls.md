---
title: "Functions and calls"
description: "Define functions with <code>let</code>, call them normally, and use <code>let rec</code> for recursion."
group: "Core language"
section: "Core language"
order: 5
slug: "functions-and-calls"
summary: "Functions, calls, and recursion without extra control syntax."
---

## What
Functions are values you can bind, pass, and call.

## Why
This gives a direct way to organize repeated logic without relying on special syntax blocks.

## How
Call named functions like usual value calls, then define recursion in the same surface with `let rec`.

## Compare
{{example:double-function}}

{{snippet:recursive-case}}

## When
Use recursion for traversals, accumulations, and simple parsers when a loop is not needed.

## Analogy
Like defining a helper in Python and calling itself from its own body, but in one value-binding style.

## Try it
Bind and call a function, then add the recursive form and continue to [Imports and packages](/docs/imports-and-packages).
