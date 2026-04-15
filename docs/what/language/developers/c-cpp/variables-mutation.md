---
title: "Variables and Mutation"
description: "Translate reassignment-heavy C and C++ code into explicit Musi mutation and fresh values."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 4
slug: "variables-mutation"
summary: "Use mutable bindings for real changing state and fresh names for calculation stages."
---

# Variables and Mutation

C and C++ allow reassignment by default. Musi makes mutation explicit at the binding site. A mutable binding says "this name is allowed to change".

C makes the changing counter ordinary:

```c
int visits = 0;
visits = visits + 1;

int next_visits = visits + 1;
```

C++ looks similar:

```cpp
auto visits = 0;
visits = visits + 1;

auto next_visits = visits + 1;
```

Musi marks the binding that can be reassigned.

{{snippet:c-cpp-variables-mutation}}

This is useful for small counters and local state. It should not become the default style for every calculation.

## Prefer fresh values when the old value still matters

Many C and C++ variables are reassigned because a function is written as a sequence of statements. In Musi, a fresh name often reads better.

C can write a calculation without reassignment:

```c
int base_price = 1200;
int total = base_price + 45;
```

C++ can do the same:

```cpp
auto base_price = 1200;
auto total = base_price + 45;
```

{{snippet:c-cpp-fresh-value}}

Use mutation when the program is truly modeling changing state. Use fresh values when the code is just naming stages of a calculation.
