---
title: "Overview"
description: "Map Lua 5.4.8 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "Lua Developers"
order: 1
slug: "overview"
summary: "Use this guide when Lua habits make Musi feel surprising."
---

Lua teaches direct tables, small functions, metatables, coroutines, nil, and host embedding. Musi keeps the small-language feel, but refuses to put every idea into a table convention. Record shape, data variants, behavior classes, effects, and foreign declarations stay separate so the compiler can follow them.

Use this guide as a translation journal for Lua code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Locals, and Expressions](/learn/guides/lua/values-locals-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Functions, Closures, and Named Calls](/learn/guides/lua/functions-closures-named-calls) - values habits get translated once, then the Musi Book owns the details.
- [Blocks, Branching, and Repetition](/learn/guides/lua/blocks-branching-repetition) - blocks habits get translated once, then the Musi Book owns the details.
- [Tables, Records, and Field Updates](/learn/guides/lua/tables-records-field-updates) - records habits get translated once, then the Musi Book owns the details.
- [Arrays, Sequences, Maps, and Pipelines](/learn/guides/lua/arrays-sequences-maps-pipelines) - arrays habits get translated once, then the Musi Book owns the details.
- [Nil, Option, and Result](/learn/guides/lua/nil-option-result) - absence and failure stop being side channels.
- [Errors, Pcall, and Effects](/learn/guides/lua/errors-pcall-effects) - outside work becomes a named request.
- [Coroutines and Effect Boundaries](/learn/guides/lua/coroutines-effect-boundaries) - outside work becomes a named request.
- [Metatables, Classes, Instances, and Laws](/learn/guides/lua/metatables-classes-instances-laws) - records habits get translated once, then the Musi Book owns the details.
- [Pattern Matching and Data Variants](/learn/guides/lua/pattern-matching-data-variants) - patterns habits get translated once, then the Musi Book owns the details.
- [Modules, Imports, and Exports](/learn/guides/lua/modules-imports-exports) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/lua/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Embedding, C API, and FFI](/learn/guides/lua/embedding-c-api-ffi) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the Lua habit of using small names and simple values.
- Drop table/metatable shortcuts when Musi can name shape, behavior, absence, or host calls directly.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is not a metatable pattern; records/data hold table-like shape, classes/instances name promised behavior. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a Lua habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
