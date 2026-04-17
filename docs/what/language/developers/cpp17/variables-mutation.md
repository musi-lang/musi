---
title: "Variables and Mutation"
description: "Translate C++17 const and mutable locals into Musi stable bindings and explicit mut values."
group: "Musi for Developers"
section: "C++ Developers"
order: 4
slug: "variables-mutation"
summary: "C++ mutable locals map to explicit Musi mut values or fresh derived names."
---

# Variables and Mutation

C++ separates stable values from changing locals with `const`:

```cpp
auto visits = 0;
visits = visits + 1;

const auto next_visits = visits + 1;
```

Musi flips the default and marks only the changing value.

{{snippet:cpp17-variables-mutation}}

When a value is only a derived step, C++ often names another local:

```cpp
const auto base_price = 1200;
const auto total = base_price + 45;
```

Musi uses the same fresh-name style without marking either binding mutable.

{{snippet:cpp17-fresh-value}}
