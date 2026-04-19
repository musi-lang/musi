---
title: "Collections, LINQ, and Pipelines"
description: "Read Collections, LINQ, and Pipelines as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 6
slug: "collections-linq-pipelines"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

LINQ can make a pipeline read like a sentence, but it can also hide where the shape changes. Musi examples name each domain value so the pipeline does not become the explanation.

{{compare:csharp-collections-linq-pipelines}}

## Reading Collections, LINQ, and Pipelines from C#

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Dot calls](/learn/book/core/dot-calls)
