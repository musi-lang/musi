---
title: "Blocks and Control Flow"
description: "Read Blocks and Control Flow as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 3
slug: "blocks-control-flow"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this branching and block results example make visible?

{{compare:java-blocks-control-flow}}

## Reading Blocks and Control Flow from Java

On the Musi side, Musi blocks and matches are expressions when they produce a value; the branch answer matters more than the statement container. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not carry over temporary variables whose only job was to smuggle a branch result out of a statement block. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use this shape when a route fee, access decision, or small rule table chooses one value from several cases. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
- [Patterns](/learn/book/data/patterns)
- [Operators](/learn/book/core/operators)
