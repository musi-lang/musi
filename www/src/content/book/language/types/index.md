---
title: "Types"
description: "Add type information, understand inference, and write small generic helpers."
group: "Types"
section: "Types"
order: 5
slug: "types"
summary: "Introduce types gradually: explicit first, inferred second, generic third."
---

This part adds type information gradually.
You will start with explicit annotations, then see where inference reduces repetition, then use generics to reuse one definition across many types.
The emphasis stays on readable, practical code rather than abstract type-system tour.

## What

This section teaches three related tools: annotations for explicit boundaries, inference for obvious cases, and generics for reusable typed definitions.
Together they answer how Musi code stays clear as values and functions become less trivial.

## Why

Users need enough type detail to write and read real code, but too much theory too early causes drop-off.
This section aims for decision-making help: when should I annotate, when can I omit, and when is a generic definition worth it?
That is more useful than a giant catalog of type features.

## How

Learn one move at a time.
Add types where clarity rises, remove only what surrounding code makes obvious, and introduce generic parameters only after you can already read annotated functions comfortably.
Keep examples small enough that every type choice still has an obvious reason.
