---
title: "Collections and Pipelines"
description: "Read Collections and Pipelines as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 6
slug: "collections-pipelines"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

A Python reader brings habits from names, dictionaries, classes, protocols, exceptions, None, imports, and duck typing. That helps with code around domain words before machinery, but the Musi page asks a narrower question: what contract should this lists, slices, maps, and pipelines example make visible?

{{compare:python-collections-pipelines}}

## Reading Collections and Pipelines from Python

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Dot calls](/learn/book/core/dot-calls)
