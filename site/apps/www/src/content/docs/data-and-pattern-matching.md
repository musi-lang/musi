---
title: "Data and pattern matching"
description: "Define sums with <code>data</code>, construct variants, and read them with <code>case</code>."
group: "Core language"
section: "Core language"
order: 7
slug: "data-and-pattern-matching"
summary: "Data definitions, constructors, and pattern matching."
---

## What
Use `data` to model bounded domains directly in code.
The `case` form reads shape by shape and keeps branching explicit.

## Why
Named variants make domain rules obvious and easier to test.
You avoid stringly-typed flags and boolean ambiguity.

## How
Define variants once, construct values from those constructors, then consume them with `case`.

{{snippet:data-port}}

{{snippet:data-port-value}}

## When
Reach for this chapter when values have distinct outcomes (task states, command result states, protocol messages).

{{snippet:data-port-case}}

## Analogy
Like a JavaScript union with explicit branches, but checked and navigated with one `case` expression.

## Try it
Read the three snippets in order, then continue to [Records and arrays](/docs/records-arrays-and-mutation).
