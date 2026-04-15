---
title: "Type Tests and Casts"
description: "Check and narrow values with explicit type-facing expressions."
group: "Types"
section: "Types"
order: 21
slug: "type-tests-and-casts"
summary: "Use `:?` to test a value against a type and `:?>` for an explicit cast."
---

Type tests ask whether a value has a shape. Casts state that code wants to treat a value as that shape and deserve attention near dynamic or native boundaries.

{{snippet:chapter-type-tests-and-casts}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Forall Types](/learn/book/types/foundations/forall-types).
