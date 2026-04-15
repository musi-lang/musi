---
title: "Arrays, Pointers, and Slices"
description: "Translate array, pointer, span, and range habits into Musi collections, pipelines, and FFI boundaries."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 6
slug: "arrays-pointers-slices"
summary: "Use collections and pipelines in ordinary code; keep raw pointers at native boundaries."
---

# Arrays, Pointers, and Slices

C arrays and pointers are close to memory. C++ adds containers, iterators, spans, and ranges. Musi ordinary code should start with collections and pipelines, not raw addresses.

C array code often mixes traversal with indexing:

```c
int ports[] = {3000, 8080};
int visible[] = {ports[0], ports[1], 9000};
```

C++ containers make the data shape safer, but the operation is still commonly written as mutation on a collection:

```cpp
std::vector<int> ports = {3000, 8080};
ports.push_back(9000);
```

Musi can read the collection transformation left to right.

{{snippet:c-cpp-arrays-pointers-slices}}

The pipeline reads left to right:

1. start with two ports;
2. append one more;
3. collect the final iterable value.

## Raw pointers belong at native boundaries

Use `@std/ffi` and `unsafe { ... }` for C ABI pointer work. Keep the raw boundary narrow, then return ordinary Musi data to the rest of the package.

C pointer offset code belongs near the native boundary:

```c
int *next = counter + 1;
```

C++ pointer arithmetic has the same risk profile:

```cpp
int *next = counter + 1;
```

In Musi, keep that kind of operation behind `unsafe { ... }` and `@std/ffi`.
