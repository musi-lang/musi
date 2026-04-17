---
title: "Overview"
description: "Translate C99 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "C Developers"
order: 1
slug: "overview"
summary: "Start from C99 habits, then read equivalent Musi expression, data, effect, package, and FFI shapes."
---

# Musi for C Developers

C99 is the comparison point for this guide. C developers often bring translation units, headers, structs, arrays, pointers, macros, explicit status codes, and ABI boundaries. Musi keeps those jobs recognizable while making values, data shape, and outside work explicit.

C usually starts with a small function and a call:

```c
int total(int base_price, int fee) {
    return base_price + fee;
}

int answer = total(1200, 45);
```

Musi gives the function the same inputs and result, then lets the body expression produce the value.

{{snippet:c99-values-functions}}

## Reading path

Read the guide from everyday C toward deeper translation points:

1. values, functions, blocks, and mutation;
2. structs, arrays, pointers, null, results, and effects;
3. variants, generics, classes, and receiver calls;
4. packages, tests, unsafe code, and native FFI boundaries.
