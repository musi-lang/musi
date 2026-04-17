---
title: "Unsafe, FFI, and Native Boundaries"
description: "Translate C99 ABI declarations and pointer operations into Musi foreign declarations and unsafe wrappers."
group: "Musi for Developers"
section: "C Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
summary: "Keep raw native calls and pointer work behind narrow unsafe boundaries."
---

# Unsafe, FFI, and Native Boundaries

C99 native boundaries pass C ABI values through declared functions:

```c
extern int puts(const char *message);

int announce(const char *message) {
    return puts(message);
}
```

Musi marks native boundaries with `foreign` and `unsafe`.

{{snippet:c99-unsafe-ffi}}

C pointer arithmetic is ordinary syntax:

```c
int *pointer = 0;
int *same_pointer = pointer + 0;
int is_null = same_pointer == 0;
```

Musi routes raw pointer work through `@std/ffi` helpers.

{{snippet:c99-ffi-pointer}}
