---
title: "Callable types"
description: "Read pure and effectful function types as part of ordinary type syntax."
group: "Types"
section: "Types"
order: 18
slug: "callable-types"
summary: "Use `T -> U` for pure callables and `T ~> U` for effectful callables."
---

{{snippet:chapter-callable-types}}

## In this chapter

Callable types describe values that can be called.
`T -> U` describes a callable from `T` to `U`.
`T ~> U` marks a callable whose evaluation may involve effects.

## Why it matters

Function definitions show parameters and result types, but APIs often need to talk about function values directly.
When a value accepts or returns a callable, the type spelling tells readers whether ordinary evaluation is enough or effect handling may matter.

## Walk through it

Read `Int -> Int` as a pure transformation from one integer to another.
Read `Int ~> Int` as the same input and output shape with effectful evaluation.
The tilde is the visible clue that capability flow can matter.

## Try it next

- Annotate one value with a pure callable type.
- Annotate one value with an effectful callable type.
- Explain which one can request effects while running.

## Common mistake

Do not read `~>` as a different argument shape. It marks effectful evaluation, not a different number of inputs.

## Next

Continue to [Type inference](/docs/language/types/type-inference) to see when annotations can be omitted.
