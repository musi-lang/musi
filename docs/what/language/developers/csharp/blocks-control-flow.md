---
title: "Blocks and Control Flow"
description: "Read Blocks and Control Flow as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 3
slug: "blocks-control-flow"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

A C# reader brings habits from nullable flow analysis, records, LINQ, async tasks, extension methods, interfaces, namespaces, and attributes. That helps with domain code through named APIs and tooling feedback, but the Musi page asks a narrower question: what contract should this branching and block results example make visible?

{{compare:csharp-blocks-control-flow}}

## Reading Blocks and Control Flow from C#

On the Musi side, Musi blocks and matches are expressions when they produce a value; the branch answer matters more than the statement container. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not carry over temporary variables whose only job was to smuggle a branch result out of a statement block. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use this shape when a route fee, access decision, or small rule table chooses one value from several cases. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
- [Patterns](/learn/book/data/patterns)
- [Operators](/learn/book/core/operators)
