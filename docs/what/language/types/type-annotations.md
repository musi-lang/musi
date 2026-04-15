---
title: "Type annotations"
description: "Use `:` for annotations, `where` for constraints, and constructor-style payloads for variants without mixing them up."
group: "Types"
section: "Types"
order: 17
slug: "type-annotations"
summary: "Learn the visible boundary markers for values, constraints, callable types, and data variants."
---

{{snippet:chapter-type-annotations}}

This chapter exists because several pieces of Musi use punctuation that looks related at first glance.
The good news is that the roles separate cleanly once you read them by context.

## In this chapter

Here are the core forms to recognize:

- annotation: `value : T`
- pure callable type: `T -> U`
- effectful callable type: `T ~> U`
- labeled callable type: `(port : Int, secure : Bool) -> Response`
- implements constraint: `where T : Eq`
- subtype constraint: `where T <: Number`
- variant payload definition: `| Configured(port : Int)`

Variant payloads use constructor-style declarations such as `| Configured(port : Int)`.
Inside that payload list, `:` labels a payload field and its type.

## Why it matters

Without this separation, readers ask reasonable questions such as:

- is `Configured : Int` an annotation?
- how is a variant payload different from `x : Int`?
- why does `where T : Eq` use `:` too?

The answer is context.
Musi keeps `:` for "name relates to type-like thing" contexts, but the surrounding form tells you which job it is doing.
`where` marks constraints.
Constructor-style payload syntax marks variants.
Plain expression position marks ordinary annotations.

## Walk through it

Read each form with its own sentence:

- `x : Int` means x has type Int.
- `(port : Int) -> Response` means a callable accepts one Int parameter labelled `port`.
- `where T : Eq` means T must implement Eq.
- `where T <: Number` means T must be below Number in subtype relation.
- `| Configured(port : Int)` means this variant carries a payload field named `port` of type `Int`.

A useful comparison for C-like readers:

- annotations feel closest to typed variable declarations
- constraints are more like generic bounds
- variant payloads are closer to constructor signatures

## What Musi does instead

Musi keeps variant payloads close to constructor syntax.
Tuple types and tuple values both use parentheses and commas. Sum types use `+` where a type needs to describe a choice between alternatives.

## Try it next

- Annotate one binding and one function result.
- Write one generic helper with `where T : Eq`.
- Define one variant with a named payload field.

## Common mistake

Do not read every `:` the same way without looking at the surrounding form.
In Musi, context is the real disambiguator.

## Next

Continue to [Type inference](/docs/language/types/type-inference) to learn when Musi can fill in the obvious parts for you.
