---
title: "Overview"
group: "Musi for Developers"
section: "C++17 Developers"
order: 1
slug: "overview"
---

# Musi for C++17 Developers

C++17 gives you RAII, classes, templates, namespaces, `std::optional`, `std::variant`, and header-based libraries. Musi keeps the taste for strong modeling and zero-surprise APIs, but uses expression syntax, records, data variants, effects, package imports, and classes/laws for reusable behavior.

## Main shifts

- Header/source organization becomes package exports and imports.
- `std::optional<T>` becomes `option.Option[T]`.
- `std::variant` becomes data variants plus `match`.
- Exceptions usually become `result.Result[T, E]` or explicit effects.
- Native and ABI work stays behind `unsafe` and `@std/ffi`.

{{snippet:cpp17-values-functions}}
