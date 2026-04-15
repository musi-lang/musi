---
title: "Native, Unsafe, and FFI"
description: "Translate Java native boundaries into Musi foreign declarations, FFI types, pointer helpers, and unsafe blocks."
group: "Musi for Developers"
section: "Java Developers"
order: 14
slug: "native-unsafe-ffi"
summary: "Keep ABI calls explicit and convert native shapes into ordinary Musi values near the boundary."
---

# Native, Unsafe, and FFI

Java native boundaries use `native` declarations and host runtime support:

```java
final class NativeConsole {
    static native int puts(String message);
}
```

Musi declares the C ABI boundary with `foreign "c"` and calls it in an unsafe block.

{{snippet:java-native-unsafe-ffi}}

## Pointer helpers

Java ordinary code does not expose raw pointer arithmetic. JNI and native libraries can still produce raw memory boundaries.

```java
long pointer = nativePointer();
boolean isNull = pointer == 0L;
```

Musi keeps raw pointer work in `@std/ffi` helpers.

{{snippet:java-ffi-pointer}}

