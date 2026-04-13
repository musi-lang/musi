---
title: "Data and pattern matching"
description: "Define sums with <code>data</code>, construct variants, and read them with <code>case</code>."
group: "Core language"
section: "Core language"
order: 8
slug: "data-and-pattern-matching"
summary: "Data definitions, constructors, and pattern matching."
---

Use `data` to model bounded domains directly in code.
The `case` form reads shape by shape and keeps branching explicit.

## Match first

{{snippet:data-port-case}}

Use `data` when a value can be one of several known shapes.

## Define and construct

{{snippet:data-port}}

{{snippet:data-port-value}}

## Compare

{{example:data-named-record}}

## Try it

{{try:data-and-pattern-matching}}

## Next step

Read the three snippets in order, then continue to [Records and arrays](/docs/records-arrays-and-mutation).
