---
title: "Functions"
description: "Write one reusable function and read its shape without extra abstraction noise."
group: "Core syntax"
section: "Core syntax"
order: 9
slug: "functions"
summary: "Define plain functions with let before learning calls or methods."
---

{{snippet:chapter-functions}}

## What

A function in Musi is still a `let` binding, but now the bound thing takes inputs and returns a result.
That continuity matters: you are not leaving basic syntax behind, only adding parameters and usually a result type.
The page's example stays tiny on purpose so the function shape is easier to see than the arithmetic.

## Why

People ask "how do I reuse logic?" almost immediately after first successful file.
A helpful answer should show that functions are ordinary named values with a clearer shape, not a new top-level declaration family to memorize.
That keeps the learning curve flatter when later chapters add calls, methods, or recursion.

## How

Read `let twice (x : Int) : Int := x + x;` as three pieces: function name, parameter list, and result expression.
Then read `twice(21);` as proof that function definition and use stay close together.
When writing your own first functions, keep one parameter, one small body, and one obvious return value until the shape feels automatic.

## Try it

- Write one one-argument function.
- Call it once with literal input.
- Rename function or parameter to make intent clearer.

## Common mistake

Do not jump to methods, classes, or generic helpers before plain function flow feels normal.

## Next

Continue to [Calls](/docs/language/core/calls) to focus on what function application looks like in everyday code.
