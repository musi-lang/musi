---
title: "Records and arrays"
description: "Use record literals, arrays, and explicit spread forms."
group: "Core language"
section: "Core language"
order: 9
slug: "records-arrays-and-mutation"
summary: "Structured values and the current writeable-data surface."
---

Records and arrays are ordinary values with predictable update patterns.

{{snippet:record-array}}

## Update by copy

{{snippet:spread-record-array}}

## Compare

{{example:record-array-spread}}

## Musi note
Musi also supports nested record update syntax such as `let r := p.{ x := 3 };` and `let r := p.{ ...q, y := 9 };`.
That form is separate from the cross-language comparison above and comes from the same record-update family used in F# and OCaml.

## Try it

{{try:records-arrays-and-mutation}}

## Next step

Create a base value then build one spread-based variant, then continue to [Types and generics](/docs/types).
