---
title: "Null, Option, and Result"
group: "Musi for Developers"
section: "C++17 Developers"
order: 7
slug: "null-option-result"
---

# Null, Option, and Result

C++17 pointer nullability and `std::optional<T>` map to `option.Option[T]`. Fallible return values map to `result.Result[T, E]` unless an effect better describes the operation.

{{snippet:cpp17-null-option}}
