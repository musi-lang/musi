---
title: "Variables and Mutation"
description: "Read Variables and Mutation as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 4
slug: "variables-mutation"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C reader brings habits from headers, translation units, pointers, arrays, `errno`, sentinel returns, and small functions that trust callers. That helps with cost, layout, and the exact boundary where bytes cross into code, but the Musi page asks a narrower question: what contract should this fresh values and mutation example make visible?

{{compare:c99-variables-mutation}}

## Reading Variables and Mutation from C99

On the Musi side, Musi makes mutation explicit with `mut` and assignment; ordinary `let` names read as stable facts. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate every rebinding habit into mutation. A new receipt, label, or counter snapshot can be a fresh name. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use mutation when the domain really changes over time, such as queue depth or a buffer cursor. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Mutation](/learn/book/start/mutation)
- [Values and let](/learn/book/start/values-and-let)
- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
