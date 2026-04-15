---
title: "Unsafe, FFI, and Native Boundaries"
description: "Translate native interop into Musi foreign declarations, FFI types, pointer helpers, and unsafe blocks."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
summary: "Keep ABI calls explicit and convert native shapes into ordinary Musi values near the boundary."
---

# Unsafe, FFI, and Native Boundaries

C and C++ are common native interop targets. Musi keeps native calls explicit with `foreign`, C-shaped types from `@std/ffi`, and `unsafe { ... }` around operations that cross the safe boundary.

C exposes an ABI function directly:

```c
int puts(const char *message);
```

C++ uses `extern "C"` when it needs a C ABI boundary:

```cpp
extern "C" int puts(const char *message);
```

Musi declares the same C ABI boundary with `foreign "c"` and calls it in an unsafe block.

{{snippet:c-cpp-unsafe-ffi}}

This keeps the ABI declaration and unsafe call site visible. Wrap native calls in small Musi functions, then return ordinary Musi values to the rest of the package.

## Pointer helpers

C pointer helpers are ordinary pointer operations:

```c
int *same_pointer = pointer + 0;
bool is_null = same_pointer == NULL;
```

C++ can use `nullptr`, but the raw pointer operation is still native memory work:

```cpp
int *same_pointer = pointer + 0;
bool is_null = same_pointer == nullptr;
```

Musi keeps pointer construction and pointer arithmetic inside `@std/ffi` helpers.

{{snippet:c-cpp-ffi-pointer}}

Pointer work should stay near the FFI edge. Convert native status codes, null pointers, and buffers into `Option`, `Result`, records, or arrays before passing data deeper into application code.
