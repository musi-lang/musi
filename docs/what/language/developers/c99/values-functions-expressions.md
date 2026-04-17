---
title: "Values, Functions, and Expressions"
description: "Translate C99 declarations, functions, and named arguments into Musi expression-oriented bindings."
group: "Musi for Developers"
section: "C Developers"
order: 2
slug: "values-functions-expressions"
summary: "C functions and call sites map to Musi let functions, expression bodies, and named calls."
---

# Values, Functions, and Expressions

C99 separates declarations, statements, and expressions. A value usually leaves a function through `return`:

```c
int total(int base_price, int fee) {
    return base_price + fee;
}

int answer = total(1200, 45);
```

Musi keeps the same input and output types, but the body is the produced value.

{{snippet:c99-values-functions}}

C call sites depend on parameter order:

```c
int render(int port, int secure) {
    return port;
}

int selected = render(8080, 1);
```

Musi can name arguments where the call benefits from visible roles.

{{snippet:c99-named-calls}}
