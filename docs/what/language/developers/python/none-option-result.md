---
title: "None, Option, and Result"
description: "Read None, Option, and Result as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 7
slug: "none-option-result"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

Python None is useful but quiet. Musi Option makes absence a branch the caller sees before touching the value.

{{compare:python-none-option-result}}

## Reading None, Option, and Result from Python

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Effects](/learn/book/effects-runtime/effects)
