---
title: "Type Inference"
description: "Learn how inference reduces repetition without hiding too much."
group: "Types"
section: "Types"
order: 19
slug: "type-inference"
summary: "See what Musi can infer so you know when to write less."
---

Type inference lets Musi fill in types that are already clear from nearby code. Annotate public boundaries and surprising local values.

{{snippet:chapter-type-inference}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Generics](/learn/book/types/foundations/generics).
