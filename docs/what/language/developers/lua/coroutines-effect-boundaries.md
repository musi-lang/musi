---
title: "Coroutines and Effect Boundaries"
description: "Read Coroutines and Effect Boundaries as a Lua habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Lua Developers"
order: 9
slug: "coroutines-effect-boundaries"
summary: "Translate the Lua habit, then use the Musi Book for the full rule."
---

Lua coroutines separate pause and resume, but they do not by themselves describe which outside service is being requested. Musi effects name that service boundary.

{{compare:lua-coroutines-effect-boundaries}}

## Reading Coroutines and Effect Boundaries from Lua

On the Musi side, Musi names outside work with effects and `request`, instead of hiding time, files, console work, or services inside an ordinary-looking call. Read the shared example through Lua eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not make an effect look pure. A clock, process, network service, or console changes the trust boundary. For a Lua reader, the trap is letting table shape, missing fields, and behavior conventions blur together; Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior.

## When this pays off

Use effects when the program asks the runtime, OS, user, or another service for an answer. The Lua instinct still helps here: Keep the Lua habit of using small names and simple values.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
