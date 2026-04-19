---
title: "Overview"
description: "Map Java 17 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "Java Developers"
order: 1
slug: "overview"
summary: "Use this guide when Java habits make Musi feel surprising."
---

Java makes code durable through packages, classes, interfaces, records, exceptions, annotations, and tools that preserve names for years. Musi keeps the concern for contracts, but shifts where contracts live: records/data for shapes, classes/instances for behavior, Option/Result-like values for expected branches, and effects for work Java often hides behind services or exceptions.

Use this guide as a translation journal for Java code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Methods, and Expressions](/learn/guides/java/values-methods-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks and Control Flow](/learn/guides/java/blocks-control-flow) - blocks habits get translated once, then the Musi Book owns the details.
- [Variables and Mutation](/learn/guides/java/variables-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Records, Classes, and Objects](/learn/guides/java/records-classes-objects) - records habits get translated once, then the Musi Book owns the details.
- [Collections, Streams, and Pipelines](/learn/guides/java/collections-streams-pipelines) - arrays habits get translated once, then the Musi Book owns the details.
- [Null, Option, and Result](/learn/guides/java/null-option-result) - absence and failure stop being side channels.
- [Exceptions and Effects](/learn/guides/java/exceptions-effects) - outside work becomes a named request.
- [Sealed Types and Patterns](/learn/guides/java/sealed-types-patterns) - patterns habits get translated once, then the Musi Book owns the details.
- [Generics, Interfaces, and Laws](/learn/guides/java/generics-interfaces-laws) - generics habits get translated once, then the Musi Book owns the details.
- [Methods and Receiver Calls](/learn/guides/java/methods-and-receiver-calls) - calls habits get translated once, then the Musi Book owns the details.
- [Packages and Modules](/learn/guides/java/packages-modules) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/java/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Native, Unsafe, and FFI](/learn/guides/java/native-unsafe-ffi) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the Java habit of naming APIs for future readers.
- Drop class-as-container reflexes when a Musi record, data variant, or instance is the smaller contract.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is a behavior contract supplied by instances; records and data model object shape. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a Java habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
