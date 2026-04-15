---
title: "Data definitions"
description: "Define record-shaped and variant-shaped data with payloads and defaults."
group: "Data"
section: "Data"
order: 15
slug: "data-definitions"
summary: "Use `data` for named shapes, variant choices, payload fields, and record defaults."
---

{{snippet:chapter-data-definitions}}

## In this chapter

A `data` block describes a reusable data shape.
Variant data uses `|` arms for choices.
Record-shaped data uses named fields separated by semicolons, and fields may have defaults.

## Why it matters

Users need more than record literals and match examples.
They need to know where the shape itself lives.
`data` gives that shape a name, whether the value is one of several variants or a record with fixed fields.

## Walk through it

Read `| Configured(port : Int, secure : Bool)` as a constructor with named payload fields.
Construction then uses those names: `.Configured(port := 8080, secure := .True)`.
Read `port : Int := 3000;` in record-shaped data as a field with a default value.

## Try it next

- Define one variant with a named payload.
- Construct it with named arguments.
- Define one record-shaped data type with one default field.

## Common mistake

Do not treat variant payload fields as ordinary record fields. Variants describe choices; records describe a fixed shape.

## Next

Continue to [Arrays and slices](/docs/language/data/arrays-and-slices) to compare named shapes with ordered sequence data.
