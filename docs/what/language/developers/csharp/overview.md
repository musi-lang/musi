---
title: "Overview"
description: "Translate .NET 8.0 and C# 12.0 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "C# Developers"
order: 1
slug: "overview"
summary: "Start from .NET 8.0 and C# 12.0 habits, then read the equivalent Musi expression, data, effect, and package shapes."
---

.NET 8.0 and C# 12.0 are the comparison points for this guide. Each page starts with C# code, then shows Musi code with the same names and the same job.

C# readers often bring methods, classes, records, nullable references, exceptions, async tasks, interfaces, extension methods, namespaces, and native interop. Musi keeps those jobs recognizable while making values, effects, and data shape explicit.

## First translation

C# can write a small expression-bodied method:

```csharp
static int Total(int basePrice, int fee) => basePrice + fee;

var answer = Total(1200, 45);
answer;
```

Musi writes the same calculation as a `let` function. The final expression leaves the value.

{{snippet:csharp-values-methods-expressions}}

Read it like a service counter: arguments arrive, one expression calculates the receipt, and the final value leaves the counter.

## Reading path

- [Values, Methods, and Expressions](/learn/book/developers/guides/csharp/values-methods-expressions)
- [Blocks and Control Flow](/learn/book/developers/guides/csharp/blocks-control-flow)
- [Variables and Mutation](/learn/book/developers/guides/csharp/variables-mutation)
- [Records, Classes, and Objects](/learn/book/developers/guides/csharp/records-classes-objects)
- [Collections, LINQ, and Pipelines](/learn/book/developers/guides/csharp/collections-linq-pipelines)
- [Null, Option, and Result](/learn/book/developers/guides/csharp/null-option-result)
- [Exceptions, Results, and Effects](/learn/book/developers/guides/csharp/exceptions-effects)
- [Unions and Pattern Matching](/learn/book/developers/guides/csharp/unions-pattern-matching)
- [Generics, Interfaces, and Laws](/learn/book/developers/guides/csharp/generics-interfaces-laws)
- [Extension Methods and Calls](/learn/book/developers/guides/csharp/extension-methods-and-calls)
- [Namespaces, Modules, and Packages](/learn/book/developers/guides/csharp/namespaces-modules-packages)
- [Testing and Tooling](/learn/book/developers/guides/csharp/testing-tooling)
- [Unsafe, Interop, and FFI](/learn/book/developers/guides/csharp/unsafe-interop-ffi)

## C# habits that transfer

- name domain types clearly
- keep boundary work visible
- use static types to explain values
- keep tests close to behavior
- use small functions when a calculation has a name

## C# habits to translate

- `return` becomes the final expression
- records and classes split into records, `data`, classes, and instances by job
- nullable values become `Option`
- ordinary recoverable exceptions become `Result`
- async and outside work become effects or stdlib runtime helpers
- interfaces become classes, instances, and laws when behavior needs rules
- native interop stays behind `foreign` and `unsafe`
