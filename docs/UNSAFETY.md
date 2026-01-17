# FFI and Memory Safety

## Overview

The language uses modifiers (`native`, `export`, `opaque`) and attributes for FFI bindings. There are no special keywords like `extern`, `declare`, `namespace`, or `module`.

Memory safety is maintained through **opaque pointers and slices** - pointer arithmetic is not allowed, and all memory access goes through bounds-checked slices.

## FFI Modifiers

### `native` - External Implementation

Declares that a function or type is implemented externally:

```musi
native fn rand(): Float[64];

native "C" fn malloc(size: Int[64]): Ptr[Nat8];
```

The optional string after `native` specifies the ABI/calling convention (e.g., `"C"`).

**Note:** Calling `native` functions is inherently unsafe (memory safety depends on correctness of external code), but no explicit `unsafe` keyword is needed. The `native` modifier itself indicates "trust me, this is correct".

### `opaque` - Hidden Implementation

Opaque type aliases hide the underlying type representation outside the defining module:

```musi
// Inside module
opaque val UserId := String;
opaque val Email := String;

val create_user_id := fn(s: String): UserId => s as UserId;
val get_id_value := fn(id: UserId): String => id as String;

// Outside module
val id := create_user_id("user123");
// id ++ "_suffix"  // ERROR: UserId is not String outside module
get_id_value(id);  // Must use provided interface
```

For FFI handles, combine with `native`:

```musi
// Native handle type
native record NativeHandle;

// Opaque FFI handles
opaque val GLTexture := NativeHandle;
opaque val FileHandle := NativeHandle;
```

### `export` - Make Visible Externally

Makes definitions visible for export (used in export statements):

```musi
fn calculate_sum(x: Int[32]): Int[32] => x * 2;

#[no_mangle]
native "C" val c_callable := fn(x: Int[32]): Bool => x % 3 = 0;

// Export at module level
export { calculate_sum, c_callable };
```

## Function Declarations vs Definitions

Functions can be declared without bodies:

```musi
// Declaration (no body)
#[link(name := "libc")]
native "C" fn external_func(x: Int[32]): Bool;

// Definition (with body)
fn local_func(x: Int[32]): Bool {
  x > 0
};

// Definition (empty body)
fn stub_func(): Int[32] {};
```

The semicolon at statement level disambiguates declarations from definitions.

## Importing and Exporting

### Importing External Symbols

```musi
import { some_func, SomeType } from "external-library";

// SAFETY: The function declarations given below are in
// line with the header files of `libc`.
#[link(name := "libc")]
native "C" fn my_c_function(x: Int[32]): Bool;
```

The `#[link]` attribute specifies which library contains the symbol. At runtime, this will attempt to link with:

- `liblibc.so` on Unix-like systems
- `libc.dll` on Windows

If the library or symbol cannot be found, the program will panic at runtime.

You can also use function type syntax:

```musi
#[link(name := "libc")]
native "C" val my_c_function: Int[32] -> Bool;
```

Both forms declare the same external function.

### Exporting to External Code

```musi
export { my_func, MyType };

#[no_mangle]
native "C" val callable_from_c := fn(x: Int[32]): Bool => x % 3 = 0;

export { callable_from_c };
```

The `#[no_mangle]` attribute preserves the function name for C linking.

If compiled as a dynamic library (dylib/shared object), the resulting `.so` or `.dll` can be linked from C code:

```c
// C code
#include <stdbool.h>

// Declare the imported function
bool callable_from_c(int32_t x);

int main() {
    bool result = callable_from_c(9);  // Returns true
    return 0;
}
```

Working with non-Musi languages and FFI is inherently unsafe, so wrappers are usually built around C APIs to provide safer interfaces.

## Memory Model: References vs Pointers

### Default: GC-Managed References

Everything is a GC-managed reference by default:

```musi
val arr := [1, 2, 3];
val same_arr := arr;  // Both reference the same array

arr.[0] <- 99;  // Mutates in place
```

No explicit pointer syntax is needed for normal code.

### Opaque Pointers for FFI

For interfacing with non-GC code (C, OpenGL, etc.), use opaque pointer types:

```musi
opaque record Ptr[T];  // Opaque pointer type - cannot be dereferenced directly

native "C" fn malloc(size: Int[64]): Ptr[Nat8];
native "C" fn free(ptr: Ptr[Nat8]);
```

**Important:** You cannot do pointer arithmetic or dereference pointers directly:

```musi
val ptr := malloc(100);
val offset := ptr + 10;  // ERROR: No pointer arithmetic allowed
val value := *ptr;       // ERROR: No dereferencing operator
```

### Safe Access Through Slices

To access memory through pointers, convert them to slices:

```musi
native fn ptr_to_slice[T](ptr: Ptr[T], len: Int[64]): []T;

val ptr := malloc(100);
val slice := ptr_to_slice[Nat8](ptr, 100);

slice.[0] <- 42;   // Safe: bounds-checked
slice.[10] <- 99;  // Safe: normal slice operations
```

All slice operations are bounds-checked at runtime.

### Automatic Marshalling at FFI Boundaries

Arrays and slices automatically marshal to pointers when calling native functions:

```musi
native "C" fn glBufferData(target: Int[32], data: []Float[32], size: Int[64]);

val vertices := [0.0, 1.0, 0.0];
glBufferData(GL_ARRAY_BUFFER, vertices, 12);  // Array auto-marshals to pointer
```

The runtime:

1. Pins the array so GC doesn't move it
2. Gets pointer to array data
3. Calls C function with pointer
4. Unpins after call returns

### Opaque Native Types

For external handles (file descriptors, GPU resources, etc.), use opaque types wrapping native records:

```musi
native record NativeHandle;

opaque val GLTexture := NativeHandle;
opaque val FileHandle := NativeHandle;

#[link(name := "OpenGL")]
native "C" fn glGenTextures(n: Int[32]): GLTexture;

#[link(name := "libc")]
native "C" fn fopen(path: []Nat8): FileHandle;
```

These are pointer-sized handles managed by external code. The opaque wrapper prevents misuse across module boundaries.

## Safety Model

### What is Safe

- All GC-managed data (arrays, records, etc.)
- Bounds-checked slice access
- Calling native functions (caller must trust the native implementation)
- Passing slices/arrays to native functions (auto-marshalled)

### What is Inherently Unsafe

- Native function implementations (external code)
- Manual memory management (malloc/free)
- Pointer lifetime management
- Race conditions in concurrent access

### Why No `unsafe` Keyword?

Since pointer arithmetic and direct dereferencing are **impossible**, and all access goes through bounds-checked slices, there's no need for an `unsafe` keyword. The `native` modifier itself indicates interaction with external, potentially unsafe code.

## Example: OpenGL Bindings

```
native record NativeHandle;

opaque val GLTexture := NativeHandle;
opaque val GLBuffer := NativeHandle;

#[link(name := "OpenGL")]
native "C" fn gl_gen_textures(n: Int[32]): GLTexture;

#[link(name := "OpenGL")]
native "C" fn gl_bind_texture(target: Int[32], texture: GLTexture);

#[link(name := "OpenGL")]
native "C" fn gl_delete_textures(texture: GLTexture);

#[link(name := "OpenGL")]
native "C" fn gl_buffer_data(target: Int[32], data: []Float[32], size: Int[64], usage: Int[32]);

// Safe wrapper
fn create_texture(): GLTexture {
  val tex := gl_gen_textures(1);
  gl_bind_texture(GL_TEXTURE_2D, tex);
  tex
};
```
