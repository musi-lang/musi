---
title: "Overview"
description: "Translate C++17 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "C++ Developers"
order: 1
slug: "overview"
summary: "Start from C++17 habits, then read equivalent Musi expression, data, effect, package, and FFI shapes."
---

# Musi for C++ Developers

C++17 is the comparison point for this guide. C++ developers often bring RAII, classes, templates, namespaces, `std::optional`, `std::variant`, exceptions, headers, and native ABI work. Musi keeps those jobs recognizable while making values, data shape, and outside work explicit.

C++ usually starts with a small function and a call:

```cpp
auto total(const int base_price, const int fee) -> int {
    return base_price + fee;
}

const auto answer = total(1200, 45);
```

Musi gives the function the same inputs and result, then lets the body expression produce the value.

{{snippet:cpp17-values-functions}}

## Reading path

Read the guide from everyday C++ toward deeper translation points:

1. values, functions, blocks, and mutation;
2. records, collections, pointers, optional values, results, and effects;
3. variants, templates, classes, and receiver calls;
4. packages, tests, unsafe code, and native FFI boundaries.
