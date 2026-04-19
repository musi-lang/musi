---
title: "Nil, Option, and Result"
description: "Read Nil, Option, and Result as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 7
slug: "nil-option-result"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

A Lua reader brings habits from tables, metatables, nil, coroutines, embedding, modules, and small runtime boundaries. That helps with simple data and host/runtime edges without much ceremony, but the Musi page asks a narrower question: what contract should this absence and failure branches example make visible?

{{compare:lua-nil-option-result}}

## Reading Nil, Option, and Result from Lua

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
