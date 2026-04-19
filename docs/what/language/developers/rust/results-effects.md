---
title: "Results, I/O, and Effects"
description: "Read Results, I/O, and Effects as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 8
slug: "results-effects"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

Rust readers already know Option and Result. In Musi examples, focus less on the concept and more on the syntax: `option.someOf`, `option.noneOf`, and pattern-shaped callers.

{{compare:rust-results-effects}}

## Reading Results, I/O, and Effects from Rust

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
