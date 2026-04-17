---
title: "Arrays, Pointers, and Slices"
description: "Translate C99 arrays, pointer decay, and length tracking into Musi collections and explicit FFI pointers."
group: "Musi for Developers"
section: "C Developers"
order: 6
slug: "arrays-pointers-slices"
summary: "Use Musi collections for safe data and reserve raw pointer work for FFI boundaries."
---

# Arrays, Pointers, and Slices

C99 keeps arrays and their lengths as separate facts:

```c
int ports[] = {3000, 8080};
size_t port_count = sizeof ports / sizeof ports[0];
int visible = ports[port_count - 1];
```

Musi keeps ordinary collections separate from native pointer work.

{{snippet:c99-arrays-pointers-slices}}
