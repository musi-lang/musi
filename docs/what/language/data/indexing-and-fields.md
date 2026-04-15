---
title: "Indexing and Fields"
description: "Read field access and indexed access without mixing up their roles."
group: "Data"
section: "Data"
order: 14
slug: "indexing-and-fields"
summary: "Use `.field` for named data and `.[index]` for positional access."
---

Field access reads named record data. Indexing reads position-based data. Use the form that matches how a reader should find the value.

{{snippet:chapter-indexing-and-fields}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Data Definitions](/learn/book/data/modeling/data-definitions).
