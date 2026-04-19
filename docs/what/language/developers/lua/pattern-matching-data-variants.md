---
title: "Pattern Matching and Data Variants"
description: "Read Pattern Matching and Data Variants as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 11
slug: "pattern-matching-data-variants"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

A Lua reader brings habits from tables, metatables, nil, coroutines, embedding, modules, and small runtime boundaries. That helps with simple data and host/runtime edges without much ceremony, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:lua-pattern-matching-data-variants}}

## Reading Pattern Matching and Data Variants from Lua

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
