---
title: "Type inference"
description: "Learn how inference reduces repetition without hiding too much."
group: "Types"
section: "Types"
order: 19
slug: "type-inference"
summary: "See what Musi can infer so you know when to write less."
---

{{snippet:chapter-type-inference}}

## In this chapter

Type inference lets Musi recover some type information from surrounding code so you do not have to repeat every obvious fact.
The example keeps one explicit `Int` annotation and then omits it on derived binding `next`.
That contrast is whole lesson: inference is convenience anchored by context, not magic guesswork.

## Why it matters

Users want shorter code, but they also want to know when shorter code stops being clear.
If docs celebrate inference without boundaries, beginners start guessing what compiler can or cannot recover.
A small example with one kept annotation and one omitted annotation teaches better instinct than a broad promise.

## Walk through it

Read annotated `port` as source of type information.
Then read `let next := port + 1;` as value whose type becomes obvious because operands already constrain it.
When editing real code, remove only the annotations that feel redundant after you can still explain the type from nearby information.

## Try it next

- Start with one annotated value.
- Add derived binding without annotation.
- Put annotation back if meaning becomes harder to read.

## Common mistake

Do not rely on inference in examples you cannot explain by tracing the surrounding code.

## Next

Continue to [Generics](/docs/language/types/generics) to reuse one definition across many types once the annotation story is clear.
