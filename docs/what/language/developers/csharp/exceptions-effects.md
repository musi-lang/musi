---
title: "Exceptions, Results, and Effects"
description: "Read Exceptions, Results, and Effects as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 8
slug: "exceptions-effects"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

A C# reader brings habits from nullable flow analysis, records, LINQ, async tasks, extension methods, interfaces, namespaces, and attributes. That helps with domain code through named APIs and tooling feedback, but the Musi page asks a narrower question: what contract should this absence and failure branches example make visible?

{{compare:csharp-exceptions-effects}}

## Reading Exceptions, Results, and Effects from C#

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
