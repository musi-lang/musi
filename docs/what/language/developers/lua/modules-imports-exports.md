---
title: "Modules, Imports, and Exports"
description: "Read Modules, Imports, and Exports as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 12
slug: "modules-imports-exports"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

A Lua reader brings habits from tables, metatables, nil, coroutines, embedding, modules, and small runtime boundaries. That helps with simple data and host/runtime edges without much ceremony, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:lua-modules-imports-exports}}

## Reading Modules, Imports, and Exports from Lua

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
