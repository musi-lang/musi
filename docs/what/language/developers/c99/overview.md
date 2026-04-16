---
title: "Overview"
group: "Musi for Developers"
section: "C99 Developers"
order: 1
slug: "overview"
---

# Musi for C99 Developers

C99 gives you translation units, headers, structs, arrays, pointers, macros, and explicit status-code conventions. Musi keeps the same preference for predictable data and visible boundaries, but moves everyday code toward expression values, variants, records, effects, and package imports.

## Main shifts

- C headers become Musi package exports.
- Status codes become `result.Result[T, E]` or effect requests.
- `NULL` checks become `option.Option[T]`.
- Pointer-heavy APIs stay behind `unsafe` and `@std/ffi`.
- Macros and generated declarations are better represented with Musi comptime syntax.

{{snippet:c99-values-functions}}
