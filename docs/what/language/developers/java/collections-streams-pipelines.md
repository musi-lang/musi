---
title: "Collections, Streams, and Pipelines"
description: "Read Collections, Streams, and Pipelines as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 6
slug: "collections-streams-pipelines"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this lists, slices, maps, and pipelines example make visible?

{{compare:java-collections-streams-pipelines}}

## Reading Collections, Streams, and Pipelines from Java

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Dot calls](/learn/book/core/dot-calls)
