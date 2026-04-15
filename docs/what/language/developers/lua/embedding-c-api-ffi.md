---
title: "Embedding, C API, and FFI"
description: "Translate Lua embedding and C API boundaries into Musi foreign declarations, FFI types, and unsafe blocks."
group: "Musi for Developers"
section: "Lua Developers"
order: 14
slug: "embedding-c-api-ffi"
summary: "Keep ABI calls explicit and convert native shapes into ordinary Musi values near the boundary."
---

# Embedding, C API, and FFI

Lua is often embedded in a host program. C boundaries use the Lua C API or native functions:

```lua
local native = require("native_console")
native.puts("ready")
```

Musi declares the C ABI boundary with `foreign "c"` and calls it in an unsafe block.

{{snippet:lua-ffi-boundary}}

## Pointer helpers

Lua user data and host objects can wrap native memory. The raw memory boundary belongs near FFI code.

```lua
local pointer = native.nullPointer()
local isNull = pointer == nil
```

Musi keeps raw pointer work in `@std/ffi` helpers.

{{snippet:lua-ffi-pointer}}
