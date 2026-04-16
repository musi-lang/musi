---
title: "Unsafe, FFI, and Native Boundaries"
group: "Musi for Developers"
section: "C99 Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
---

# Unsafe, FFI, and Native Boundaries

C99 is the natural ABI boundary. Keep raw C pointers, C strings, and native calls behind small Musi wrappers.

{{snippet:c99-unsafe-ffi}}
