---
title: "Unsafe, Cgo, and FFI"
description: "Translate Go unsafe and cgo boundaries into Musi foreign declarations, FFI types, pointer helpers, and unsafe blocks."
group: "Musi for Developers"
section: "Go Developers"
order: 15
slug: "unsafe-cgo-ffi"
summary: "Keep ABI calls explicit and convert native shapes into ordinary Musi values near the boundary."
---

# Unsafe, Cgo, and FFI

Go cgo and `unsafe` make native boundaries explicit:

```go
/*
#include <stdio.h>
*/
import "C"

func announce(message *C.char) C.int {
    return C.puts(message)
}
```

Musi declares the C ABI boundary with `foreign "c"` and calls it in an unsafe block.

{{snippet:go-unsafe-ffi}}

## Pointer helpers

Go ordinary code avoids pointer arithmetic. Native libraries and `unsafe.Pointer` can still cross raw memory boundaries.

```go
var pointer unsafe.Pointer
isNull := pointer == nil
```

Musi keeps raw pointer work in `@std/ffi` helpers.

{{snippet:go-ffi-pointer}}
