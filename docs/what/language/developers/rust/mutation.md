---
title: "Mutation"
description: "Read Mutation as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 3
slug: "mutation"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

A Rust reader brings habits from ownership, enums, traits, Result, modules, pattern matching, and explicit unsafe blocks. That helps with the type that carries the invariant, but the Musi page asks a narrower question: what contract should this fresh values and mutation example make visible?

{{compare:rust-mutation}}

## Reading Mutation from Rust

On the Musi side, Musi makes mutation explicit with `mut` and assignment; ordinary `let` names read as stable facts. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate every rebinding habit into mutation. A new receipt, label, or counter snapshot can be a fresh name. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use mutation when the domain really changes over time, such as queue depth or a buffer cursor. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Mutation](/learn/book/start/mutation)
- [Values and let](/learn/book/start/values-and-let)
- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
