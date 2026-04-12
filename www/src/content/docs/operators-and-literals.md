---
title: "Operators and literals"
description: "Read numbers, strings, records, arrays, and operator chains in the same surface."
group: "Core language"
section: "Core language"
order: 6
slug: "operators-and-literals"
summary: "Literal forms and operator precedence in ordinary expressions."
---

## What
Operators and literals are the small pieces you combine to express computation directly.
They are the ingredients you mix before a function or `case` branch takes over.

## When
Use them for numbers, text, comparisons, and small inline data values.

## Why
This keeps calculations close to the data they shape, which makes code easier to scan.

It also avoids common confusion by making operator meaning explicit in one place.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Use literal forms directly, then combine them with the operators Musi supports:
- numeric literals like `8080`
- string literals like `"ready"`
- symbolic operators like `+`, `-`, `*`, `/`, `=`, `<=`, `>=`, and `/=`
- word-like operators like `and`, `or`, `not`, `shl`, `shr`, and `xor`

### Operator intent
- `and`, `or`, `not`, and `xor` are word operators whose meaning depends on operand types and operator definitions in scope.
- `shl` and `shr` are shift operators.
- `/=` means not equal. It is not divide-and-assign.

If you are coming from languages where `/=` means divide-and-assign, use `x := x / y` when you want reassignment from division.

{{snippet:operators-literals-basic}}

{{snippet:record-array}}

{{snippet:spread-record-array}}

## Analogy
Think of a recipe card: literals are the ingredients on the counter, and operators are the steps that mix or compare them.

## Try it
Combine a few literals and operators in one file, then continue to [Functions and calls](/docs/functions-and-calls).
