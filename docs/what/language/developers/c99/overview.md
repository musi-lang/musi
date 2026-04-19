---
title: "Overview"
description: "Map C99 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "C Developers"
order: 1
slug: "overview"
summary: "Use this guide when C99 habits make Musi feel surprising."
---

C99 teaches you to notice layout, lifetime, pointer ownership, and linker boundaries before you trust an abstraction. Bring that suspicion to Musi, but let the source name more of the contract: imports replace header folklore, data variants replace tag comments, Option replaces null sentinels, and effects/foreign declarations mark work that leaves ordinary evaluation.

Use this guide as a translation journal for C99 code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Functions, and Expressions](/learn/guides/c99/values-functions-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks and Control Flow](/learn/guides/c99/blocks-control-flow) - blocks habits get translated once, then the Musi Book owns the details.
- [Variables and Mutation](/learn/guides/c99/variables-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Structs, Classes, and Records](/learn/guides/c99/structs-classes-records) - records habits get translated once, then the Musi Book owns the details.
- [Arrays, Pointers, and Slices](/learn/guides/c99/arrays-pointers-slices) - arrays habits get translated once, then the Musi Book owns the details.
- [Null, Option, and Result](/learn/guides/c99/null-option-result) - absence and failure stop being side channels.
- [Errors, Results, and Effects](/learn/guides/c99/errors-results-effects) - outside work becomes a named request.
- [Enums, Variants, and Patterns](/learn/guides/c99/enums-variants-patterns) - patterns habits get translated once, then the Musi Book owns the details.
- [Macros, Generics, Classes, and Laws](/learn/guides/c99/templates-concepts-classes-laws) - generics habits get translated once, then the Musi Book owns the details.
- [Methods and Receiver Calls](/learn/guides/c99/methods-and-receiver-calls) - calls habits get translated once, then the Musi Book owns the details.
- [Headers, Modules, and Packages](/learn/guides/c99/headers-modules-packages) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/c99/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Unsafe, FFI, and Native Boundaries](/learn/guides/c99/unsafe-ffi-native-boundaries) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the C habit of asking where memory, symbols, and failures come from.
- Drop the habit of encoding meaning in integer flags, comments, or pointer conventions when Musi has a type for that meaning.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a C99 habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
