---
title: "Functions, Closures, and Named Calls"
description: "Read Functions, Closures, and Named Calls as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 3
slug: "functions-closures-named-calls"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

A Lua reader brings habits from tables, metatables, nil, coroutines, embedding, modules, and small runtime boundaries. That helps with simple data and host/runtime edges without much ceremony, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:lua-functions-closures-named-calls}}

## Reading Functions, Closures, and Named Calls from Lua

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Values and let](/learn/book/start/values-and-let)
- [Functions](/learn/book/core/functions)
- [Calls](/learn/book/core/calls)
