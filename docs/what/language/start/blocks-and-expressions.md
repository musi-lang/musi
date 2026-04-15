---
title: "Blocks and expressions"
description: "Learn how blocks produce results, why Musi does not need `return`, and how to model repetition without loop statements."
group: "Start"
section: "Start"
order: 4
slug: "blocks-and-expressions"
summary: "Treat a block as one expression with setup at the top and the result at the bottom."
---

{{snippet:chapter-blocks-and-expressions}}

A Musi block groups several steps, but it still behaves like one expression.
That one idea explains a lot of the language.
It is why blocks, functions, and match arms all feel related instead of like disconnected grammar features.

## In this chapter

Inside a block, earlier lines can prepare values and later lines can use them.
The last expression becomes the value of the whole block.

That means this block model already answers questions many users bring from other languages:

- Where does the result come from? The last expression.
- Where is `return`? Usually nowhere. It is not needed.
- How do I do several steps? Put them in a block and end with the value you want.

## Why it matters

If you keep expecting statement braces from C, JavaScript, or Java, Musi blocks will feel strange.
If you accept that blocks are expressions, many later features suddenly become easier:

- `match` arms return values naturally
- handler clauses return values naturally
- helper blocks inside functions stay readable
- recursion becomes a more natural replacement for loop statements

Musi does not have `for`, `while`, `break`, or `continue` statements.
Instead, cycling work usually comes from recursion, ranges, or higher-order helpers from the standard library.
That only feels reasonable once expression flow is clear.

## Walk through it

Read the example line by line.
Ask what each line introduces, then ask what the final line computes from those earlier names.

A good habit is to paraphrase a block like this:

- first, bind `base`
- then, derive another value from it
- finally, produce the result

That paraphrase scales from tiny arithmetic blocks to real code.

## Try it next

- Write one block with two helper bindings and one final expression.
- Replace the last line and predict the new result before running it.
- Rewrite a tiny loop-shaped idea as a recursive helper plus a block result.

## Common mistake

Do not assume only a keyword such as `return` can make a block meaningful.
In Musi, the final expression already does that job.

## Next

Continue to [Mutation](/docs/language/start/mutation) to see what changes when state is meant to vary instead of being rebound once.
