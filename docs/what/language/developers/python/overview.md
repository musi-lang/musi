---
title: "Overview"
description: "Map Python 3.14 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "Python Developers"
order: 1
slug: "overview"
summary: "Use this guide when Python habits make Musi feel surprising."
---

Python favors readable names, dictionaries, classes, exceptions, None, imports, and duck typing. Musi keeps the readable domain-first style, but asks more facts to be written down: records/data for shape, classes/instances for promised behavior, Option for expected absence, and effects for work that Python often hides behind a call that may raise.

Use this guide as a translation journal for Python code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Functions, and Final Expressions](/learn/guides/python/values-functions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks, Control Flow, and Repetition](/learn/guides/python/blocks-control-flow) - blocks habits get translated once, then the Musi Book owns the details.
- [Names, Mutation, and Fresh Values](/learn/guides/python/names-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Records, Objects, and Dictionaries](/learn/guides/python/records-objects-dicts) - records habits get translated once, then the Musi Book owns the details.
- [Collections and Pipelines](/learn/guides/python/collections-pipelines) - arrays habits get translated once, then the Musi Book owns the details.
- [None, Option, and Result](/learn/guides/python/none-option-result) - absence and failure stop being side channels.
- [Exceptions, Results, and Effects](/learn/guides/python/exceptions-effects) - outside work becomes a named request.
- [Data Variants and Pattern Matching](/learn/guides/python/data-variants-patterns) - patterns habits get translated once, then the Musi Book owns the details.
- [Types, Generics, and Protocols](/learn/guides/python/types-generics-protocols) - generics habits get translated once, then the Musi Book owns the details.
- [Modules and Packages](/learn/guides/python/modules-packages) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/python/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Native Boundaries, Unsafe, and FFI](/learn/guides/python/native-unsafe-ffi) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the Python habit of naming the real-world thing first.
- Drop implicit duck-typing and None sentinels when the Musi type can say the branch upfront.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a Python habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
