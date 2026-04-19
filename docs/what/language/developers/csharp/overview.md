---
title: "Overview"
description: "Map .NET 8.0 / C# 12.0 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "C# Developers"
order: 1
slug: "overview"
summary: "Use this guide when C# habits make Musi feel surprising."
---

C# trains readers to expect strong tooling, namespaces, nullable annotations, records, interfaces, LINQ, tasks, and attributes. Musi uses similar clarity but different boxes: records/data hold shape, classes and instances hold behavior, Option names absence, and effects make async or runtime work visible instead of hiding it behind a task-returning method.

Use this guide as a translation journal for C# code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Methods, and Expressions](/learn/guides/csharp/values-methods-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks and Control Flow](/learn/guides/csharp/blocks-control-flow) - blocks habits get translated once, then the Musi Book owns the details.
- [Variables and Mutation](/learn/guides/csharp/variables-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Records, Classes, and Objects](/learn/guides/csharp/records-classes-objects) - records habits get translated once, then the Musi Book owns the details.
- [Collections, LINQ, and Pipelines](/learn/guides/csharp/collections-linq-pipelines) - arrays habits get translated once, then the Musi Book owns the details.
- [Null, Option, and Result](/learn/guides/csharp/null-option-result) - absence and failure stop being side channels.
- [Exceptions, Results, and Effects](/learn/guides/csharp/exceptions-effects) - outside work becomes a named request.
- [Unions and Pattern Matching](/learn/guides/csharp/unions-pattern-matching) - patterns habits get translated once, then the Musi Book owns the details.
- [Generics, Interfaces, and Laws](/learn/guides/csharp/generics-interfaces-laws) - generics habits get translated once, then the Musi Book owns the details.
- [Extension Methods and Calls](/learn/guides/csharp/extension-methods-and-calls) - calls habits get translated once, then the Musi Book owns the details.
- [Namespaces, Modules, and Packages](/learn/guides/csharp/namespaces-modules-packages) - mutation habits get translated once, then the Musi Book owns the details.
- [Testing and Tooling](/learn/guides/csharp/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Unsafe, Interop, and FFI](/learn/guides/csharp/unsafe-interop-ffi) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the C# habit of making API shape readable at the call site.
- Drop the assumption that every reusable behavior needs an object hierarchy or service object.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a C# habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
