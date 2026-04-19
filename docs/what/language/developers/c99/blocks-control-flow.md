---
title: "Blocks and Control Flow"
description: "Read Blocks and Control Flow as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 3
slug: "blocks-control-flow"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C reader brings habits from headers, translation units, pointers, arrays, `errno`, sentinel returns, and small functions that trust callers. That helps with cost, layout, and the exact boundary where bytes cross into code, but the Musi page asks a narrower question: what contract should this branching and block results example make visible?

{{compare:c99-blocks-control-flow}}

## Reading Blocks and Control Flow from C99

On the Musi side, Musi blocks and matches are expressions when they produce a value; the branch answer matters more than the statement container. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not carry over temporary variables whose only job was to smuggle a branch result out of a statement block. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use this shape when a route fee, access decision, or small rule table chooses one value from several cases. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
- [Patterns](/learn/book/data/patterns)
- [Operators](/learn/book/core/operators)
