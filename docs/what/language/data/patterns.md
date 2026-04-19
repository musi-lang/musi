---
title: "Patterns"
description: "Learn pattern matching after records and arrays, not before."
group: "Data"
section: "Data"
order: 14
slug: "patterns"
summary: "Use match and destructuring to branch on data shape."
---
Patterns let code inspect the shape of a value. They are most useful with data definitions, records, tuples, and cases where the next step depends on what kind of value arrived.

{{snippet:chapter-patterns}}

A pattern should read like unpacking a box. First identify the kind of box, then name the pieces you need. If a piece is not needed, do not invent a distracting name.

## Matching real situations

A support ticket may be open, waiting, or closed. A delivery may be at the warehouse, in transit, or delivered. Pattern matching lets each case explain its own response without mixing all branches into one pile of conditions.

## Patterns are not guesses

Prefer patterns when the value already has a known shape. Do not use pattern matching to compensate for unclear data. If every branch is full of unrelated checks, the data model may need better variants.

Data chapters teach the shape of a room before anyone moves through it. A record is a labeled card, an array is an ordered row, a variant is a set of named doors, and a pattern is a safe way to open one door at a time.

Most data bugs start when one shape is asked to mean too many things. If a value can be absent, use an absent-value shape. If a value has named parts, use a record. If a value can be one of several cases, use data variants.
