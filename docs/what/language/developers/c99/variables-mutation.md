---
title: "Variables and Mutation"
description: "Translate C99 mutable locals into Musi stable bindings and explicit mut values."
group: "Musi for Developers"
section: "C Developers"
order: 4
slug: "variables-mutation"
summary: "C mutable locals map to explicit Musi mut values or fresh derived names."
---

# Variables and Mutation

C locals are mutable ordinary variables:

```c
int visits = 0;
visits = visits + 1;

int next_visits = visits + 1;
```

Musi makes the changing value explicit.

{{snippet:c99-variables-mutation}}

When a value is only a derived step, C often still names another local:

```c
int base_price = 1200;
int total = base_price + 45;
```

Musi uses the same fresh-name style without marking either binding mutable.

{{snippet:c99-fresh-value}}
