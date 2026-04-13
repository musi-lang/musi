---
title: "Records"
description: "Use records for labeled data before mixing in arrays or pattern matching."
group: "Data"
section: "Data"
order: 12
slug: "records"
summary: "Build named-field values and access fields directly."
---

{{snippet:record-array}}

{{snippet:spread-record-array}}

## What

Records group named fields into one value.

## Why

Field names make structure visible without adding too much syntax at once.

## How

- Build a record with `{ field := value }`.
- Read fields with dot access.
- Use spread when building an updated copy helps.

## Try it

- Make one record with two fields.
- Read one field.
- Build a spread-updated copy.

## Common mistake

Do not treat records as magical namespaces first.

## Next

Continue to [Arrays and slices](/docs/language/data/arrays-and-slices).
