---
title: "Values, Functions, and Expressions"
description: "Translate C and C++ function habits into Musi expression-bodied functions and named calls."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 2
slug: "values-functions-expressions"
summary: "Use expression bodies, positional calls, and named calls for readable functions."
---

# Values, Functions, and Expressions

C starts with statements and function bodies. C++ adds richer expressions, but many everyday functions still end with an explicit `return`. Musi treats ordinary code as expression-oriented: a function body produces the value written after `:=`.

A C function might read like this:

```c
int total(int base_price, int fee) {
    return base_price + fee;
}
```

C++ may keep the same function shape or bind a callable object:

```cpp
auto total = [](int base_price, int fee) -> int {
    return base_price + fee;
};
```

The Musi equivalent keeps the same inputs and output, but the body is the expression itself.

{{snippet:c-cpp-values-functions}}

There is no `return` keyword in ordinary Musi functions. The function body is already the returned value.

## Calls can stay positional or become named

C and C++ calls are usually positional. That is compact, but it can hide what boolean or numeric arguments mean:

```c
int selected = render(8080, true);
```

```cpp
auto selected = render(8080, true);
```

Musi supports named calls when the call reads better with labels.

{{snippet:c-cpp-named-calls}}

Use positional calls for small obvious values. Use named calls when numbers, booleans, or repeated types would make the call easy to misread.
