---
title: "Enums, Data, and Pattern Matching"
description: "Read Enums, Data, and Pattern Matching as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 5
slug: "enums-data"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

A Rust reader brings habits from ownership, enums, traits, Result, modules, pattern matching, and explicit unsafe blocks. That helps with the type that carries the invariant, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:rust-enums-data}}

## Reading Enums, Data, and Pattern Matching from Rust

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
