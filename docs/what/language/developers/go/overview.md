---
title: "Overview"
description: "Map Go 1.26.2 habits to the Musi Book without repeating every construct example."
group: "Musi for Developers"
section: "Go Developers"
order: 1
slug: "overview"
summary: "Use this guide when Go habits make Musi feel surprising."
---

Go rewards small packages, plain structs, explicit errors, interfaces by behavior, and concurrency that stays close to call sites. Musi keeps direct naming, but moves more facts into declarations: Option for absence, data for variants, classes/instances for shared behavior, and effects for operations that ask the outside world.

Use this guide as a translation journal for Go code. Start with the familiar habit, then ask which Musi construct owns the same promise. A page is successful when it tells you which old instinct to keep and which old shortcut to drop.

## Start from the habit that hurts

- [Values, Functions, and Final Expressions](/learn/guides/go/values-functions-expressions) - values habits get translated once, then the Musi Book owns the details.
- [Blocks, Branching, and Loops](/learn/guides/go/blocks-branching-loops) - blocks habits get translated once, then the Musi Book owns the details.
- [Variables, Short Declarations, and Mutation](/learn/guides/go/variables-short-declarations-mutation) - mutation habits get translated once, then the Musi Book owns the details.
- [Structs, Records, and Field Updates](/learn/guides/go/structs-records-field-updates) - records habits get translated once, then the Musi Book owns the details.
- [Slices, Arrays, Maps, and Pipelines](/learn/guides/go/slices-arrays-maps-pipelines) - arrays habits get translated once, then the Musi Book owns the details.
- [Nil, Option, and Result](/learn/guides/go/nil-option-result) - absence and failure stop being side channels.
- [Errors, Results, and Effects](/learn/guides/go/errors-results-effects) - outside work becomes a named request.
- [Goroutines, Channels, Context, and Effects](/learn/guides/go/goroutines-channels-context-effects) - outside work becomes a named request.
- [Interfaces, Classes, Instances, and Laws](/learn/guides/go/interfaces-classes-laws) - behavior contracts stop hiding in object habits.
- [Generics and Type Constructors](/learn/guides/go/generics-type-constructors) - generics habits get translated once, then the Musi Book owns the details.
- [Methods and Receiver Calls](/learn/guides/go/methods-receiver-calls) - calls habits get translated once, then the Musi Book owns the details.
- [Packages, Imports, and Exports](/learn/guides/go/packages-imports-exports) - package boundaries replace compatibility-era habits.
- [Testing and Tooling](/learn/guides/go/testing-tooling) - testing habits get translated once, then the Musi Book owns the details.
- [Unsafe, Cgo, and FFI](/learn/guides/go/unsafe-cgo-ffi) - native edges stay narrow enough to audit.

## Habits that still help

- Keep the Go habit of writing the small thing first and naming package boundaries clearly.
- Drop nil and second-return conventions when Musi can put the branch in the type.
- Keep examples tied to ordinary work: receipts, routes, files, animals, people, rooms, and services.

## First false friend

Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention. If you need a person, car, invoice, or package, start with records or data. If you need any type that can compare, show, encode, or step, use a class with instances.

## When to switch to the Musi Book

Stay in this guide while a Go habit is getting in the way. Switch to [Values and let](/learn/book/start/values-and-let), [Records](/learn/book/data/records), [Patterns](/learn/book/data/patterns), [Generics](/learn/book/types/generics), [Classes](/learn/book/abstractions/classes), and [Effects](/learn/book/effects-runtime/effects) when you need the source-of-truth rule.
