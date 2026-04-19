---
title: "Sealed Types and Patterns"
description: "Read Sealed Types and Patterns as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 9
slug: "sealed-types-patterns"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:java-sealed-types-patterns}}

## Reading Sealed Types and Patterns from Java

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
