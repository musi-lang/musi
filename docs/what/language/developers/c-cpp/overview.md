---
title: "Overview"
description: "Translate C23 and C++23 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 1
slug: "overview"
summary: "Start from C23 and C++23 habits, then read equivalent Musi expression, data, effect, and FFI shapes."
---

# Musi for C/C++ Developers

This guide is written for C23 and C++23 developers. It treats C and C++ as close neighbors with different habits:

- C developers often think in headers, structs, functions, pointers, explicit status codes, and ABI boundaries.
- C++ developers often think in classes, constructors, destructors, templates, overloads, RAII, exceptions, and concepts.
- Musi keeps values, data definitions, functions, generic constraints, effects, and FFI boundaries visible in the source.

The closest Musi starting point for a C/C++ developer is not "classes first" or "pointers first". It is "model the value first, then make the boundary explicit".

A small C boundary often starts as a plain function:

```c
int total(int base_price, int fee) {
    return base_price + fee;
}
```

C++ can express the same calculation with a typed callable value:

```cpp
auto total = [](int base_price, int fee) -> int {
    return base_price + fee;
};
```

Musi keeps the inputs and output visible, but the function body is already the produced value.

{{snippet:c-cpp-values-functions}}

## Habits that transfer well

C and C++ both reward careful ownership of boundaries. That habit carries over directly:

- Keep package interfaces small.
- Name data by domain role, not by storage trick.
- Put native boundaries behind explicit functions.
- Keep mutation local when a value must change.
- Use tests near the behavior they protect.

## Habits to translate

Some familiar shapes mean different things in Musi:

- A block is an expression, so it can produce a value.
- `match` handles branching over variants and ordinary values.
- recursive bindings name repetition directly instead of hiding repetition behind loop syntax.
- `Option` replaces nullable absence in ordinary Musi code.
- `Result` represents expected failure as a value.
- Effects model outside work such as console interaction.
- `unsafe { ... }` marks native and raw operations.

## Reading path

Read the guide in order if you are moving a C or C++ mental model into Musi:

1. Values, functions, and expressions.
2. Blocks and control flow.
3. Variables and mutation.
4. Records, variants, generics, classes, and laws.
5. Modules, packages, tests, unsafe, and FFI.
