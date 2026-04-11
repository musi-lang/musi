---
title: "Foundation and standard library"
description: "Know when to use <code>@std</code> and when you are looking at lower-level foundation names."
group: "Tooling"
section: "Tooling"
order: 14
slug: "foundation-and-standard-library"
summary: "The standard library family and the lower-level foundation namespace."
---

## What
Most user code starts in `@std`.
`musi:*` is the lower-level family when you need foundation-level capabilities.

## Why
Keeping both namespaces explicit makes the dependency model visible:
- `@std` for everyday work
- `musi:*` for core-level operations

## How
Import from `@std` modules first, then layer `musi:*` only where the project surface needs it.

{{snippet:stdlib-option-import}}

{{snippet:stdlib-result-import}}

## Compare
{{example:import-stdlib}}

## When
Use `@std` for normal application logic.
Use `musi:*` for boundary-focused tooling and low-level integration tasks.

## Analogy
Like choosing a standard library versus runtime SDK in other ecosystems.

## Try it
Move one project import to `@std`, then continue to [Testing and running](/docs/testing-and-running).
