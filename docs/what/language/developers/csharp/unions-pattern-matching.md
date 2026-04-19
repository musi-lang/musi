---
title: "Unions and Pattern Matching"
description: "Read Unions and Pattern Matching as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 9
slug: "unions-pattern-matching"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

A C# reader brings habits from nullable flow analysis, records, LINQ, async tasks, extension methods, interfaces, namespaces, and attributes. That helps with domain code through named APIs and tooling feedback, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:csharp-unions-pattern-matching}}

## Reading Unions and Pattern Matching from C#

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
