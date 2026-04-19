---
title: "Enums, Variants, and Patterns"
description: "Read Enums, Variants, and Patterns as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 9
slug: "enums-variants-patterns"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

A C++ reader brings habits from RAII, overload sets, templates, references, optional values, exceptions, and object hierarchies. That helps with ownership stories, value categories, and compile-time abstractions, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:cpp17-enums-variants-patterns}}

## Reading Enums, Variants, and Patterns from C++17

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
