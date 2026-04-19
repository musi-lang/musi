---
title: "Values, Functions, and Final Expressions"
description: "Read Values, Functions, and Final Expressions as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 2
slug: "values-functions"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

A Rust reader brings habits from ownership, enums, traits, Result, modules, pattern matching, and explicit unsafe blocks. That helps with the type that carries the invariant, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:rust-values-functions}}

## Reading Values, Functions, and Final Expressions from Rust

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Values and let](/learn/book/start/values-and-let)
- [Functions](/learn/book/core/functions)
- [Calls](/learn/book/core/calls)
