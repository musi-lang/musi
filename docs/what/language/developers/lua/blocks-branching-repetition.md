---
title: "Blocks, Branching, and Repetition"
description: "Read Blocks, Branching, and Repetition as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 4
slug: "blocks-branching-repetition"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

A Lua reader brings habits from tables, metatables, nil, coroutines, embedding, modules, and small runtime boundaries. That helps with simple data and host/runtime edges without much ceremony, but the Musi page asks a narrower question: what contract should this branching and block results example make visible?

{{compare:lua-blocks-branching-repetition}}

## Reading Blocks, Branching, and Repetition from Lua

On the Musi side, Musi blocks and matches are expressions when they produce a value; the branch answer matters more than the statement container. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not carry over temporary variables whose only job was to smuggle a branch result out of a statement block. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use this shape when a route fee, access decision, or small rule table chooses one value from several cases. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
- [Patterns](/learn/book/data/patterns)
- [Operators](/learn/book/core/operators)
