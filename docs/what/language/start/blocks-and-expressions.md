---
title: "Blocks and Expressions"
description: "Learn how blocks produce results, why Musi does not need `return`, and how to model repetition without loop statements."
group: "Start"
section: "Start"
order: 4
slug: "blocks-and-expressions"
summary: "Treat a block as one expression with setup at the top and the result at the bottom."
---

A block groups several steps and produces one value. That makes blocks useful anywhere Musi expects an expression.

{{snippet:chapter-blocks-and-expressions}}

Read the block like a small workbench. `base` and `offset` are tools laid out inside the bench. `base + offset` is the item that leaves the bench. The surrounding code sees only the final value.

This matters because the same block shape appears in function bodies, match arms, handlers, and unsafe scopes. Once "last expression wins" feels natural, many later features become smaller.

## Local Names

Names introduced inside a block stay inside that block. Use a block when a calculation needs helper names but outside code should only see the result.

Continue to [Mutation](/learn/book/start/foundations/mutation).
