---
title: "Native Boundaries, Unsafe, and FFI"
description: "Translate Python C extension and ctypes boundary habits into Musi foreign declarations, Ptr values, and unsafe blocks."
group: "Musi for Developers"
section: "Python Developers"
order: 13
slug: "native-unsafe-ffi"
summary: "Keep native calls explicit with foreign declarations, @std/ffi types, and unsafe blocks."
---

Python can cross into native code through extension modules, `ctypes`, or `cffi`. That boundary deserves a signpost because the host language cannot protect every operation there.

```python
from ctypes import CDLL, c_char_p

libc = CDLL("libc.so.6")
puts = libc.puts
puts.argtypes = [c_char_p]
puts.restype = int
```

Musi declares the foreign function and keeps the call behind `unsafe`.

{{snippet:python-native-unsafe-ffi}}

`foreign "c"` says which native calling world the declaration belongs to. `unsafe { ... }` marks the operation that needs extra care.

## Pointer helpers

Use `@std/ffi` pointer helpers when the native boundary gives you pointer-shaped values.

{{snippet:python-ffi-pointer}}

Keep raw native work small. Convert it to ordinary Musi data before passing values deeper into the program.
