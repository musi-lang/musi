---
title: "Callable Types"
description: "Read pure and effectful function types as part of ordinary type syntax."
group: "Types"
section: "Types"
order: 18
slug: "callable-types"
summary: "Use `T -> U` for pure callables and `T ~> U` for effectful callables."
---

Callable types describe functions as values. `Int -> Int` is pure shape; `Int ~> Int` is effectful callable shape.

{{snippet:chapter-callable-types}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Type Tests and Casts](/learn/book/types/foundations/type-tests-and-casts).
