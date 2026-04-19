---
title: "Null, Option, and Result"
description: "Read Null, Option, and Result as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 7
slug: "null-option-result"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

Optional values, nullable pointers, and exceptions solve different C++ problems. Musi keeps expected absence in Option and leaves effects or results for work that can fail for a reason.

{{compare:cpp17-null-option-result}}

## Reading Null, Option, and Result from C++17

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
