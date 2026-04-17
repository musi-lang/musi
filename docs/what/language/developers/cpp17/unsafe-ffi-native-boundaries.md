---
title: "Unsafe, FFI, and Native Boundaries"
description: "Translate C++17 native wrappers and pointer operations into Musi foreign declarations and unsafe wrappers."
group: "Musi for Developers"
section: "C++ Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
summary: "Keep raw native calls and pointer work behind narrow unsafe boundaries."
---

# Unsafe, FFI, and Native Boundaries

C++17 crosses stable ABI boundaries through `extern "C"` wrappers:

```cpp
extern "C" auto puts(const char* message) -> int;

extern "C" auto announce(const char* message) -> int {
    return puts(message);
}
```

Musi marks native boundaries with `foreign` and `unsafe`.

{{snippet:cpp17-unsafe-ffi}}

C++ pointer arithmetic is ordinary syntax:

```cpp
const auto* pointer = static_cast<const int*>(nullptr);
const auto* same_pointer = pointer + 0;
const auto is_null = same_pointer == nullptr;
```

Musi routes raw pointer work through `@std/ffi` helpers.

{{snippet:cpp17-ffi-pointer}}
