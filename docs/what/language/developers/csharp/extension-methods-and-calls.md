---
title: "Extension Methods and Calls"
description: "Read Extension Methods and Calls as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 11
slug: "extension-methods-and-calls"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

A C# reader brings habits from nullable flow analysis, records, LINQ, async tasks, extension methods, interfaces, namespaces, and attributes. That helps with domain code through named APIs and tooling feedback, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:csharp-extension-methods-and-calls}}

## Reading Extension Methods and Calls from C#

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Calls](/learn/book/core/calls)
- [Dot calls](/learn/book/core/dot-calls)
- [Functions](/learn/book/core/functions)
