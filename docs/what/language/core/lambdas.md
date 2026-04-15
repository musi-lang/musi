---
title: "Lambdas"
description: "Write a small function value directly inside an expression."
group: "Core syntax"
section: "Core syntax"
order: 12
slug: "lambdas"
summary: "Use lambda expressions when a short function value reads better in place."
---

{{snippet:chapter-lambdas}}

## In this chapter

A lambda is an unnamed function value.
It uses `\` before the parameter list and `=>` before the body.
That makes it useful where a helper is small enough that naming it separately would interrupt the reading flow.

## Why it matters

Higher-order helpers, callbacks, adapters, and small transformations all need function values.
A named `let` function is clearer for reused behavior.
A lambda is clearer when the behavior belongs right where it is passed or bound.

## Walk through it

Read `\(x : Int) : Int => x + x` as "given `x`, produce `x + x`."
The optional result annotation works the same way as a named function result annotation.
If the body grows beyond one direct idea, give the function a name.

## Try it next

- Bind one lambda to a name and call it.
- Rewrite the same code as a named function.
- Choose the version where the reader does less jumping around.

## Common mistake

Do not hide multi-step behavior inside a lambda when a named helper would explain the intent.

## Next

Continue to [Calls](/docs/language/core/calls) to follow arguments through functions.
