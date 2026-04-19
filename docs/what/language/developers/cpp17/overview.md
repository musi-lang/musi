---
title: "Overview"
description: "Map C++17 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "C++ Developers"
order: 1
slug: "overview"
summary: "Use this guide when C++17 habits make Musi feel surprising."
---

C++17 builds large systems from value types, RAII, templates, overloads, exceptions, and carefully hidden resource edges. Musi keeps the taste for named operations and static promises, but separates stored shape from behavior contracts: records/data describe things, classes describe operations, effects and foreign declarations mark boundary work.

Use this guide as a translation journal for C++17 code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Functions, and Expressions](/learn/guides/cpp17/values-functions-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks and Control Flow](/learn/guides/cpp17/blocks-control-flow) - blocks habits get translated once, then the Musi Book owns the details.
- [Variables and Mutation](/learn/guides/cpp17/variables-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Structs, Classes, and Records](/learn/guides/cpp17/structs-classes-records) - records habits get translated once, then the Musi Book owns the details.
- [Arrays, Pointers, and Slices](/learn/guides/cpp17/arrays-pointers-slices) - arrays habits get translated once, then the Musi Book owns the details.
- [Null, Option, and Result](/learn/guides/cpp17/null-option-result) - absence and failure stop being side channels.
- [Errors, Results, and Effects](/learn/guides/cpp17/errors-results-effects) - outside work becomes a named request.
- [Enums, Variants, and Patterns](/learn/guides/cpp17/enums-variants-patterns) - patterns habits get translated once, then the Musi Book owns the details.
- [Templates, Concepts, Classes, and Laws](/learn/guides/cpp17/templates-concepts-classes-laws) - generics habits get translated once, then the Musi Book owns the details.
- [Methods and Receiver Calls](/learn/guides/cpp17/methods-and-receiver-calls) - calls habits get translated once, then the Musi Book owns the details.
- [Headers, Modules, and Packages](/learn/guides/cpp17/headers-modules-packages) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/cpp17/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Unsafe, FFI, and Native Boundaries](/learn/guides/cpp17/unsafe-ffi-native-boundaries) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the C++ habit of asking which operation is generic and which value owns shape.
- Drop inheritance-first and overload-first reflexes when a record, data variant, or explicit instance says the same thing plainly.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a C++17 habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
