---
title: "Records and arrays"
description: "Use record literals, arrays, and explicit spread forms."
group: "Core language"
section: "Core language"
order: 9
slug: "records-arrays-and-mutation"
summary: "Structured values and the current writeable-data surface."
---

## What
Records and arrays are ordinary values with predictable update patterns.

{{snippet:record-array}}

## When
Use these forms for request payloads, config objects, and small in-memory collections.

## Why
They keep data structured and avoid mixing unrelated values into one flat tuple.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Build values with literals, then use spread/update forms when you need a modified copy.

{{snippet:spread-record-array}}

{{example:record-array-spread}}

## Musi note
Musi also supports nested record update syntax such as `let r := p.{ x := 3 };` and `let r := p.{ ...q, y := 9 };`.
That form is separate from the cross-language comparison above and comes from the same record-update family used in F# and OCaml.

## Analogy
Like object/array literals in JS, with explicit update syntax.

## Try it
Create a base value then build one spread-based variant, then continue to [Types and generics](/docs/types).
