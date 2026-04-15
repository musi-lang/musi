---
title: "Arrays and slices"
description: "Read sequence-shaped data without mixing it into pattern syntax yet."
group: "Data"
section: "Data"
order: 13
slug: "arrays-and-slices"
summary: "Store ordered values and learn where slices fit."
---

{{snippet:chapter-arrays-and-slices}}

## In this chapter

Arrays store ordered values, while slice helpers let you work with sequence-shaped data without inventing your own low-level operations each time.
This page pairs a tiny literal array with one stdlib slice example so you can see both local syntax and the broader sequence workflow.
Ordered data is common enough that users need more than a token example.

## Why it matters

Beginners quickly ask how to represent "a list of things" and how to combine or pass that list around.
If the docs only show `[1, 2, 3]`, they still do not know what happens next.
Showing array creation next to `@std/slice` answers both shape question and "what do I do with this afterward?" question.

## Walk through it

Read `[1, 2, 3]` as one ordered value where position matters more than field names.
Then read `Slice.concat[Int]([1], [2, 3]);` as a normal library call over sequence values, not special built-in mutation.
When writing real code, create arrays locally, then reach for slice helpers when you need to combine, traverse, or transform them in clearer steps.

## Try it next

- Bind one small array literal.
- Concatenate two arrays with slice helper.
- Rename arrays so their roles are obvious.

## Common mistake

Do not use arrays when named fields would explain the data better than positions.

## Next

Continue to [Patterns](/docs/language/data/patterns) to branch on data shape once records and sequences already feel familiar.
