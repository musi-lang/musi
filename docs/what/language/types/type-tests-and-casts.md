---
title: "Type tests and casts"
description: "Check and narrow values with explicit type-facing expressions."
group: "Types"
section: "Types"
order: 21
slug: "type-tests-and-casts"
summary: "Use `:?` to test a value against a type and `:?>` for an explicit cast."
---

{{snippet:chapter-type-tests-and-casts}}

## In this chapter

Type-facing expressions keep runtime checks visible.
`value :? Type` asks whether a value fits a type.
`value :?> Type` asks for an explicit cast to that type.

## Why it matters

Dynamic boundaries, foreign data, and broad APIs sometimes need a visible check before code continues.
Hiding that check inside a helper name can make the risky part disappear.
The `:?` and `:?>` forms keep the type question attached to the value being checked.

## Walk through it

Read `value :? Int` as a question.
Read `value :?> Int` as a conversion request that should only appear where the cast is justified.
When possible, prefer earlier precise types so casts stay rare.

## Try it next

- Write one type test for a value.
- Write one cast near a boundary.
- Move the cast closer to the boundary if it drifts into ordinary domain code.

## Common mistake

Do not use casts to avoid modeling data precisely. Casts should explain a boundary, not erase one.

## Next

Continue to [Forall types](/docs/language/types/forall-types) for explicit universal type forms.
