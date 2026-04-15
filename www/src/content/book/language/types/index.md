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
You start with explicit annotations, then see where inference reduces repetition, then use generics to reuse one definition across many types.
The emphasis stays on readable, practical code rather than an abstract type-system tour.

Think of types as labels, molds, and rules.
Some pages name the label on one value, some pages show the mold that fits many values, and some pages show the rule sheet that keeps callers honest.

## Path Through This Part

This section teaches three related tools: annotations for explicit boundaries, inference for obvious cases, and generics for reusable typed definitions.
It introduces callable type spelling such as `T -> U` and `T ~> U`, so readers can connect type chapters back to functions and effects.
It also makes the `:` story explicit: annotations use `:`, constraints use `where`, and named variant payloads use constructor-style declarations such as `| Configured(port : Int)`.

## What This Part Solves

Users need enough type detail to write and read real code, but too much theory too early causes drop-off.
This section aims for decision-making help: when should I annotate, when can I omit, when is a generic definition worth it, and how do callable types fit into effectful code?
That is more useful than a giant catalog of type features.

## How to Read It

Learn one move at a time.
Add types where clarity rises, remove only what surrounding code makes obvious, and introduce generic parameters only after you can already read annotated functions comfortably.
Keep examples small enough that every type choice still has an obvious reason.


After `forall`, the section introduces practical dependent types: value parameters in type lists, indexed variant results, `partial` for runtime-only definitions, and `~=` for type equality.
