---
title: "Mutation"
description: "Learn Musi's explicit mutation surface without mixing it into every lesson."
group: "Start"
section: "Start"
order: 5
slug: "mutation"
summary: "Use mut only when changing a value helps more than rebuilding it."
---

{{snippet:chapter-mutation}}

## What

Mutation in Musi is explicit.
A value becomes mutable only when you mark it with `mut`, and reassignment uses same `:=` surface in a clearly state-changing position.
That keeps changing state visible instead of quietly blending it into ordinary bindings.

## Why

New users need to know both that mutation exists and that Musi does not want it everywhere.
If docs only show immutable examples, people ask how to update counters or accumulators.
If docs present mutation as default, readers miss one of the language's clarity wins: stable values stay stable unless you opt into change.

## How

Read `let counter := mut 1;` as creation of one mutable cell with initial value `1`.
Read `counter := 2;` as reassignment of existing mutable value, not creation of a second binding.
When writing real code, start by asking whether a new immutable value would read better; choose `mut` when step-by-step updates make the intent clearer.

## Try it

- Create one mutable counter.
- Reassign it once.
- Rewrite same tiny task with immutable bindings and compare readability.

## Common mistake

Do not add `mut` automatically just because value changes in other languages.

## Next

Continue to [Literals](/docs/language/core/literals) to build up the everyday values you will bind, transform, and compare.
