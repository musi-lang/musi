---
title: "Tuples and unit"
description: "Group values by position and recognize the empty value."
group: "Core syntax"
section: "Core syntax"
order: 7
slug: "tuples-and-unit"
summary: "Use tuple expressions for small positional groups and unit when no payload matters."
---

{{snippet:chapter-tuples-and-unit}}

## In this chapter

Tuples group a few values by position. Use them when names would add noise and the order already carries the meaning.
The empty tuple `()` is unit. It is useful when a value must exist syntactically but no information needs to travel.

## Why it matters

Many readers expect every grouped value to be a record or object.
Musi gives you both choices: records for named fields, tuples for short positional data.
Knowing that split keeps small return values readable without inventing temporary record names everywhere.

## Walk through it

Read `(8080, "ready")` as two values moving together.
Read `()` as "nothing to carry" rather than as a missing value.
When data grows past two or three positions, prefer a record so readers do not have to remember what each slot means.

## Try it next

- Return two related values from one expression.
- Replace an unclear tuple with a record and compare readability.
- Use `()` when a branch or handler result only needs to signal completion.

## Common mistake

Do not use tuples as anonymous records when field names would explain the code better.

## Next

Continue to [Operators](/docs/language/core/operators) to combine values into larger expressions.
