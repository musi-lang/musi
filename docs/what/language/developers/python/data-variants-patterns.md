---
title: "Data Variants and Pattern Matching"
description: "Read Data Variants and Pattern Matching as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 9
slug: "data-variants-patterns"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

A Python reader brings habits from names, dictionaries, classes, protocols, exceptions, None, imports, and duck typing. That helps with code around domain words before machinery, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:python-data-variants-patterns}}

## Reading Data Variants and Pattern Matching from Python

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
