---
title: "Indexing and Fields"
description: "Read field access and indexed access without mixing up their roles."
group: "Data"
section: "Data"
order: 14
slug: "indexing-and-fields"
summary: "Use `.field` for named data and `.[index]` for positional access."
---
Indexing selects by position. Field access selects by name. Both are ways to move from a larger value to the part needed for the next expression.

{{snippet:chapter-indexing-and-fields}}

Choose the access style that matches the data. A row in a timetable may be indexed by slot. A booking record should use fields because the pieces have names.

## The reader's question

When reading `customer.address`, the question is answered immediately: this code needs the address. When reading `items.[2]`, the reader asks why the third item matters. Good code answers that question nearby.

## Keep paths short

Long chains of fields and indexes can make ownership unclear. If a path is important, name the intermediate value: `shippingAddress`, `firstPet`, or `primaryContact`.

Short paths also improve diagnostics and hovers. When an editor can highlight `shippingAddress.city` as a record field rather than a vague nested expression, the code becomes easier to inspect. Names are not only for humans; they give tools better handles too.

Data chapters teach the shape of a room before anyone moves through it. A record is a labeled card, an array is an ordered row, a variant is a set of named doors, and a pattern is a safe way to open one door at a time.

Most data bugs start when one shape is asked to mean too many things. If a value can be absent, use an absent-value shape. If a value has named parts, use a record. If a value can be one of several cases, use data variants.
