---
title: "Type Annotations"
description: "Use `:` for annotations, `where` for constraints, and constructor-style payloads for variants without mixing them up."
group: "Types"
section: "Types"
order: 17
slug: "type-annotations"
summary: "Learn the visible boundary markers for values, constraints, callable types, and data variants."
---

`:` marks a type relationship, but the surrounding form tells you which relationship you are reading: value annotation, parameter annotation, constraint, or payload field.

{{snippet:chapter-type-annotations}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Type Inference](/learn/book/types/foundations/type-inference).
