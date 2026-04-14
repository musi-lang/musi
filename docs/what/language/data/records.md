---
title: "Records"
description: "Use records for labeled data before mixing in arrays or pattern matching."
group: "Data"
section: "Data"
order: 12
slug: "records"
summary: "Build named-field values and access fields directly."
---

{{snippet:chapter-records}}

## What

Records let you store related values under field names.
The first snippet shows record construction with named fields, and the second shows how to build a new record from an existing one with spread update.
Together they cover the two questions beginners actually ask: how to make one, and how to change one without losing everything else.

## Why

Plain numbers and strings stop being enough as soon as data has roles such as `x`, `y`, `name`, or `port`.
If docs only mention record syntax once and move on, users still need to ask how to update one field while keeping rest intact.
Records are where labeled data starts feeling practical instead of theoretical.

## How

Read `{ x := 3, y := 4 }` as one value with two named fields.
Then read `{ ...point, z := 5 }` as "copy existing record shape, then override or add selected fields."
When writing your own records, choose field names that make access obvious and use spread when you want a new value that mostly keeps old data.

## Try it

- Create one record with two named fields.
- Build a second record with spread update.
- Change one field name or value to reflect a real domain example.

## Common mistake

Do not rebuild an entire record by hand when only one or two fields need to change.

## Next

Continue to [Arrays and slices](/docs/language/data/arrays-and-slices) to compare labeled record data with ordered sequence data.
