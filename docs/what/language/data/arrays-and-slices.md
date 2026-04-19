---
title: "Arrays and Slices"
description: "Read sequence-shaped data without mixing it into pattern syntax yet."
group: "Data"
section: "Data"
order: 13
slug: "arrays-and-slices"
summary: "Store ordered values and learn where slices fit."
---
Arrays and slices hold ordered collections. They fit lists of people in a queue, payments on a statement, animals in a shelter, or checkpoints on a route.

{{snippet:chapter-arrays-and-slices}}

Order is part of the meaning. If the order does not matter, the collection may still be an array for convenience, but the code should not rely on accidental positions.

## Working with many values

Most collection code asks one of a few questions: transform every item, keep the matching items, find one item, or combine the items into a summary. Name the helper functions after those questions.

## Indexes are fragile

An index is useful when position is the rule, such as the third seat or the next checkpoint. If the position only happens to work today, prefer a search by field or a named lookup.

Data chapters teach the shape of a room before anyone moves through it. A record is a labeled card, an array is an ordered row, a variant is a set of named doors, and a pattern is a safe way to open one door at a time.

Most data bugs start when one shape is asked to mean too many things. If a value can be absent, use an absent-value shape. If a value has named parts, use a record. If a value can be one of several cases, use data variants.
