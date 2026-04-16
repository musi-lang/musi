---
title: "Unsafe, FFI, and Native Boundaries"
group: "Musi for Developers"
section: "C++17 Developers"
order: 14
slug: "unsafe-ffi-native-boundaries"
---

# Unsafe, FFI, and Native Boundaries

C++17 native APIs should cross Musi through a narrow C-compatible ABI. Keep C++ ownership, lifetimes, and exception behavior behind wrappers.

{{snippet:cpp17-unsafe-ffi}}
