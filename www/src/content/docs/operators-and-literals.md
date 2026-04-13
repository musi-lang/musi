---
title: "Operators and literals"
description: "Read numbers, strings, records, arrays, and operator chains in the same surface."
group: "Core language"
section: "Core language"
order: 6
slug: "operators-and-literals"
summary: "Literal forms and operator precedence in ordinary expressions."
---

Operators and literals cover most day-one code: numbers, strings, arrays, records, comparisons, and small calculations.

## Common forms

- numeric literals like `8080`
- string literals like `"ready"`
- symbolic operators like `+`, `-`, `*`, `/`, `=`, `<=`, `>=`, and `/=`
- word-like operators like `and`, `or`, `not`, `shl`, `shr`, and `xor`

### Operator intent
- `and`, `or`, `not`, and `xor` are word operators whose meaning depends on operand types and operator definitions in scope.
- `shl` and `shr` are shift operators.
- `/=` means not equal. It is not divide-and-assign.

If you are coming from languages where `/=` means divide-and-assign, use `x := x / y` when you want reassignment from division.

## Examples

{{snippet:operators-literals-basic}}

{{snippet:record-array}}

{{snippet:spread-record-array}}

## Try it

{{try:operators-and-literals}}

## Next step

Combine a few literals and operators in one file, then continue to [Functions and calls](/docs/functions-and-calls).
