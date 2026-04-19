---
title: "Values, Methods, and Expressions"
description: "Read Values, Methods, and Expressions as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 2
slug: "values-methods-expressions"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:java-values-methods-expressions}}

## Reading Values, Methods, and Expressions from Java

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Values and let](/learn/book/start/values-and-let)
- [Functions](/learn/book/core/functions)
- [Calls](/learn/book/core/calls)
