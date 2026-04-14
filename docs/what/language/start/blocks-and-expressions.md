---
title: "Blocks and expressions"
description: "Learn block flow before adding mutation or larger control forms."
group: "Start"
section: "Start"
order: 4
slug: "blocks-and-expressions"
summary: "Understand how grouped expressions produce one final value."
---

{{snippet:chapter-blocks-and-expressions}}

## What

A block groups several steps and still counts as one expression.
Inside the block you can introduce helper bindings, and the last line becomes the value the whole block produces.
That makes Musi feel expression-first even when the code has multiple stages.

## Why

Beginners often expect grouped code to behave like statement braces from JavaScript or C-family languages.
That expectation causes confusion about "where does this return from?" or "why is last line not ignored?"
Learning block result flow early prevents that confusion before you meet `match`, handlers, or larger definitions built from same idea.

## How

Read the block from top to bottom.
`let base := 8000;` is local setup. `base + 80` is not a random trailing line; it is result of whole block.
When you write your own block, put setup first, keep one clear final expression last, and ask what value the whole group should produce.

## Try it

- Create one block with a helper binding.
- Put arithmetic expression last.
- Replace last line and see how block result changes.

## Common mistake

Do not read grouped Musi code as if only explicit `return` can produce a value.

## Next

Continue to [Mutation](/docs/language/start/mutation) to see what changes when a value is meant to vary over time.
