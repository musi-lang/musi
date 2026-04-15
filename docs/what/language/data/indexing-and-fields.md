---
title: "Indexing and fields"
description: "Read field access and indexed access without mixing up their roles."
group: "Data"
section: "Data"
order: 14
slug: "indexing-and-fields"
summary: "Use `.field` for named data and `.[index]` for positional access."
---

{{snippet:chapter-indexing-and-fields}}

## In this chapter

Musi uses dot syntax for two common data reads.
A named field uses `.field` after the value.
Indexed access uses `.[...]` so it stays visually separate from field names and method calls.

## Why it matters

Records and arrays answer different questions.
A field says "read this named part."
An index says "read this position."
Keeping the punctuation distinct helps readers understand whether the code depends on a name or on an order.

## Walk through it

Read `point.x` as field access on a record-like value.
Read `values.[0]` as indexed access into ordered data.
If code starts using many numeric indexes, consider naming intermediate values or changing the data shape.

## Try it next

- Build one record and read one field.
- Build one array and read one index.
- Replace a confusing index read with a named field when the domain has a clear label.

## Common mistake

Do not use positional access when a field name would carry the meaning better.

## Next

Continue to [Data definitions](/docs/language/data/data-definitions) to define reusable data shapes.
