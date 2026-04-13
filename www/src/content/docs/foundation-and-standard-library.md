---
title: "Foundation and standard library"
description: "Know when to use <code>@std</code> and when you are looking at lower-level foundation names."
group: "Tooling"
section: "Tooling"
order: 15
slug: "foundation-and-standard-library"
summary: "The standard library family and the lower-level foundation namespace."
---

Most user code starts in `@std`.
`musi:*` is the lower-level family when you need foundation-level capabilities.

## Default split

- `@std` for everyday work
- `musi:*` for core-level operations

Most code should stay in `@std`. Reach for `musi:*` when you are working near the language runtime boundary or lower-level compiler-facing tools.

{{snippet:stdlib-option-import}}

{{snippet:stdlib-result-import}}

## Compare
{{example:import-stdlib}}

## Try it

{{try:foundation-and-standard-library}}

## Next step

Move one project import to `@std`, then continue to [Testing and running](/docs/testing-and-running).
