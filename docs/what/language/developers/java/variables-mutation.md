---
title: "Variables and Mutation"
description: "Read Variables and Mutation as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 4
slug: "variables-mutation"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this fresh values and mutation example make visible?

{{compare:java-variables-mutation}}

## Reading Variables and Mutation from Java

On the Musi side, Musi makes mutation explicit with `mut` and assignment; ordinary `let` names read as stable facts. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate every rebinding habit into mutation. A new receipt, label, or counter snapshot can be a fresh name. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use mutation when the domain really changes over time, such as queue depth or a buffer cursor. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Mutation](/learn/book/start/mutation)
- [Values and let](/learn/book/start/values-and-let)
- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
