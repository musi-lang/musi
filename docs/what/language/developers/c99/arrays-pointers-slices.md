---
title: "Arrays, Pointers, and Slices"
group: "Musi for Developers"
section: "C99 Developers"
order: 6
slug: "arrays-pointers-slices"
---

# Arrays, Pointers, and Slices

C99 arrays decay to pointers in many call positions. Musi keeps collection values and native pointers separate. Use normal Musi collections for safe application data and `@std/ffi` pointer types only at native boundaries.

{{snippet:c99-arrays-pointers-slices}}
