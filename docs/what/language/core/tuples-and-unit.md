---
title: "Tuples and Unit"
description: "Group values by position and recognize the empty value."
group: "Core Syntax"
section: "Core Syntax"
order: 7
slug: "tuples-and-unit"
summary: "Use tuple expressions for small positional groups and unit when no payload matters."
---

Tuples group values by position. Unit `()` carries no information and marks completion as a value. Use records when names would prevent mistakes.

{{snippet:chapter-tuples-and-unit}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Operators](/learn/book/core/expressions/operators).
