---
title: "Tables, Records, and Field Updates"
description: "Read Tables, Records, and Field Updates as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 5
slug: "tables-records-field-updates"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

Lua tables can be arrays, maps, objects, modules, and namespaces. Musi records narrow that role to field-shaped data so behavior does not disappear into table convention.

{{compare:lua-tables-records-field-updates}}

## Reading Tables, Records, and Field Updates from Lua

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Records](/learn/book/data/records)
- [Classes](/learn/book/abstractions/classes)
- [Patterns](/learn/book/data/patterns)
