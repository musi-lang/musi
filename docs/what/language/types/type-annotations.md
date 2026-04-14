---
title: "Type annotations"
description: "Introduce type annotations before inference and generics."
group: "Types"
section: "Types"
order: 18
slug: "type-annotations"
summary: "Add type information where it helps readers and tools."
---

{{snippet:chapter-type-annotations}}

## What

Type annotations tell both reader and compiler what kind of value a binding or function expects.
This example shows them on a simple value and on a generic function, which is enough to see the surface without drowning in type theory.
Annotations are there to clarify important boundaries, not to decorate every name.

## Why

Users ask for annotations when code stops being self-evident.
A raw value such as `8080` may be obvious, but function parameters, return shapes, and reused helpers benefit from visible type anchors.
This chapter should show where annotations earn their keep before inference and generics complicate the picture.

## How

Read `let port : Int := 8080;` as ordinary binding with an explicit type promise inserted between name and value.
Then read function annotation positions separately: parameter types explain inputs, result type explains output.
When writing your own code, annotate public or non-obvious boundaries first, then stop once the code becomes clearer rather than noisier.

## Try it

- Add type annotation to one value binding.
- Add parameter or result type to one function.
- Compare readability before and after.

## Common mistake

Do not annotate everything by reflex when only a few boundaries actually need explanation.

## Next

Continue to [Type inference](/docs/language/types/type-inference) to see where Musi can safely fill in detail for you.
