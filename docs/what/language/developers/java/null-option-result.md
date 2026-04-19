---
title: "Null, Option, and Result"
description: "Read Null, Option, and Result as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 7
slug: "null-option-result"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

A Java reader brings habits from packages, classes, records, interfaces, exceptions, annotations, streams, and long-lived APIs. That helps with contracts, visibility, and maintainable names, but the Musi page asks a narrower question: what contract should this absence and failure branches example make visible?

{{compare:java-null-option-result}}

## Reading Null, Option, and Result from Java

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
