---
title: "Values, Functions, and Expressions"
description: "Translate C++17 functions, expression habits, and named arguments into Musi expression-oriented bindings."
group: "Musi for Developers"
section: "C++ Developers"
order: 2
slug: "values-functions-expressions"
summary: "C++ functions and call sites map to Musi let functions, expression bodies, and named calls."
---

# Values, Functions, and Expressions

C++17 often returns values through explicit `return` statements:

```cpp
auto total(const int base_price, const int fee) -> int {
    return base_price + fee;
}

const auto answer = total(1200, 45);
```

Musi keeps the same input and output types, but the body is the produced value.

{{snippet:cpp17-values-functions}}

C++ call sites depend on parameter order:

```cpp
auto render(const int port, const bool secure) -> int {
    return port;
}

const auto selected = render(8080, true);
```

Musi can name arguments where the call benefits from visible roles.

{{snippet:cpp17-named-calls}}
