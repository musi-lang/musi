---
title: "Arrays, Pointers, and Slices"
description: "Translate C++17 containers, arrays, iterators, and pointer work into Musi collections and explicit FFI pointers."
group: "Musi for Developers"
section: "C++ Developers"
order: 6
slug: "arrays-pointers-slices"
summary: "Use Musi collections for safe data and reserve raw pointer work for FFI boundaries."
---

# Arrays, Pointers, and Slices

C++17 often uses standard containers for growable values:

```cpp
auto ports = std::vector<int>{3000, 8080};
ports.push_back(9000);

const auto visible = ports;
```

Musi keeps ordinary collections separate from native pointer work.

{{snippet:cpp17-arrays-pointers-slices}}
