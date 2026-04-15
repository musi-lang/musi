---
title: "Unsafe, Interop, and FFI"
description: "Translate C# unsafe code and P/Invoke habits into Musi foreign declarations, Ptr values, and unsafe blocks."
group: "Musi for Developers"
section: "C# Developers"
order: 14
slug: "unsafe-interop-ffi"
summary: "Keep native calls explicit with foreign declarations, @std/ffi types, and unsafe blocks."
---

C# can cross native boundaries with P/Invoke and unsafe code:

```csharp
using System.Runtime.InteropServices;

[LibraryImport("c")]
internal static partial int puts(IntPtr message);
```

Musi declares the foreign function and keeps the call behind `unsafe`.

{{snippet:csharp-unsafe-interop-ffi}}

`foreign "c"` names the native calling world. `unsafe { ... }` marks the operation that needs extra care.

## Pointer helpers

Use `@std/ffi` pointer helpers when the native boundary gives you pointer-shaped values.

{{snippet:csharp-ffi-pointer}}

Keep raw native work small. Convert it to ordinary Musi data before passing values deeper into the program.
