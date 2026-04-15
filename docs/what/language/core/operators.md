---
title: "Operators"
description: "Add operators after literals so expressions stay readable."
group: "Core syntax"
section: "Core syntax"
order: 7
slug: "operators"
summary: "Read arithmetic, comparison, and logic in ordinary expressions."
---

{{snippet:chapter-operators}}

## In this chapter

Operators let you combine existing values into new results.
In this example, arithmetic, equality, ordering, and bit-shift forms all stay inside ordinary `let` bindings so the code still reads like plain Musi, not like a separate operator sublanguage.
The point is not memorizing every symbol at once; it is seeing operators as part of normal expression flow.

## Why it matters

Most "how do I compute this?" questions start here.
Users want to add one, compare two values, or build a flag, and they need examples that look like real code rather than isolated operator tables.
Showing operators next to named bindings answers both syntax question and readability question.

## Walk through it

Read `port + 1` as value transformation, `next = port + 1` as equality check, and `port <= 9000` as a guard-like comparison that still returns a value.
The `shl` example shows that named operator forms follow same expression pattern.
When writing your own code, start with named inputs, then build one operator expression per binding so the meaning stays obvious.

## Try it next

- Start from one numeric binding.
- Add one arithmetic result and one comparison result.
- Read both results aloud as values, not hidden control flow.

## Common mistake

Do not pack several unrelated operators into one line before each result has a clear name.

## Next

Continue to [Ranges](/docs/language/core/ranges) to see how Musi writes "from here to there" without guessing at endpoint meaning.
