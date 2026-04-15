---
title: "Forall Types"
description: "Read explicit universal type forms without turning them into everyday ceremony."
group: "Types"
section: "Types"
order: 22
slug: "forall-types"
summary: "Use `forall` when a type expression must bind a type variable explicitly."
---

`forall` writes polymorphism as a type. Use it when the type itself must say that one callable works for every type parameter.

{{snippet:chapter-forall-types}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Dependent Types](/learn/book/types/foundations/dependent-types).
