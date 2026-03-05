# Phase 11 -- FFI

**Crate:** `musi_vm`, `musi_codegen`
**Goal:** C interop via `native` + attributes, no magic.
**Dependencies:** Phase 9 (GC + closures), Phase 10 (modules)

---

## Deliverables

### `native fn` with `#[link]`

```
// Declare a foreign function
#[link("libc")]
native fn strlen(s: Ptr[Char]): Int;

#[link("libm")]
native fn sqrt(x: Float): Float;
```

- `#[link("libname")]` → dynamic library to load at runtime.
- `native fn` without `#[intrinsic]` but with `#[link]` → FFI call (not VM intrinsic).
- The compiler reads the attribute to know which library to load.

### Dynamic Symbol Resolution

```
FFIRegistry = {
  loaded_libs: HashMap<String, DynLib>,
  resolved_fns: HashMap<(String, String), FnPtr>,  // (lib, symbol) → pointer
}

fn resolve_ffi(lib_name: &str, fn_name: &str) → FnPtr:
  if not loaded_libs.contains(lib_name):
    loaded_libs[lib_name] = dlopen(lib_name)
  return dlsym(loaded_libs[lib_name], fn_name)
```

- Libraries loaded lazily on first call.
- Symbol not found → runtime error with clear message.

### `#[repr("C")]` on Records

```
#[repr("C")]
record CPoint {
  x: Float,
  y: Float,
}
```

- `#[repr("C")]` → fields laid out in declaration order with C alignment rules.
- Without `#[repr("C")]` → Musi's own layout (may differ).
- Codegen emits type metadata so VM knows how to marshal.

### `Ptr[T]` -- User-Defined with Intrinsic Ops

Declared in stdlib (not hardcoded):

```
// std/ffi.ms
#[intrinsic(ptr_type)]
export native type Ptr[T];

#[intrinsic(ptr_deref)]
export native fn deref[T](p: Ptr[T]): T;     // !p

#[intrinsic(ptr_addr_of)]
export native fn addr_of[T](x: T): Ptr[T];   // @x

#[intrinsic(ptr_null)]
export native fn null_ptr[T](): Ptr[T];

#[intrinsic(ptr_offset)]
export native fn ptr_offset[T](p: Ptr[T], n: Int): Ptr[T];
```

**No magic principle:** The compiler sees `#[intrinsic(ptr_type)]` and knows this type has special representation (raw pointer). There's no `if type_name == "Ptr"` check anywhere.

### Argument Marshaling

```
Musi Type    → C Type
Int          → int64_t
Float        → double
Bool         → uint8_t (0 or 1)
String       → const char* (null-terminated, temporary)
Ptr[T]       → T*
#[repr("C")] record → struct (matching layout)
Unit         → void (return only)
```

**String marshaling:**
- Musi strings are UTF-8, not null-terminated.
- For FFI calls: allocate temporary null-terminated copy on `MemoryStack`.
- Returned C strings: copy into Musi heap string.

### MemoryStack for Transient FFI Allocations

```
MemoryStack = {
  buffer: Vec<u8>,
  offset: usize,
}

fn alloc_temp(size: usize, align: usize) → *mut u8:
  // Bump allocator, reset after each FFI call returns
  aligned_offset = align_up(offset, align)
  ptr = &buffer[aligned_offset]
  offset = aligned_offset + size
  return ptr

fn reset():
  offset = 0
```

- Used for temporary C strings and struct copies during FFI calls.
- Reset after each FFI call returns -- no leak.

### Return Value Translation

- C return value → Musi value according to marshaling table.
- Void return → `Unit`.
- Pointer return → `Ptr[T]` (raw, user must manage lifetime).

### Error Handling

- NULL pointer dereference → runtime panic with message.
- Library not found → runtime error: "cannot load library: libname".
- Symbol not found → runtime error: "symbol not found: fn_name in libname".

### New Opcodes

```
Opcode +=
  | call.ffi(u16)          // call FFI function by index (marshaling handled by VM)
  | ld.ptr                  // load through pointer
  | st.ptr                  // store through pointer
```

---

## Milestone

1. Call `strlen("hello")` from Musi via libc → returns 5.
2. Call `sqrt(2.0)` from libm → returns ~1.414.
3. Pass a `#[repr("C")]` record to a C function.
4. `Ptr[T]` operations: addr_of, deref, null check.
5. `cargo test --workspace` passes.

### Milestone test program:

```
import { Ptr, deref, addr_of } from "std/ffi";

#[link("libc")]
native fn strlen(s: Ptr[Char]): Int;

#[link("libm")]
native fn sqrt(x: Float): Float;

writeln(int_to_string(strlen("hello")));  // 5
writeln(float_to_string(sqrt(2.0)));       // 1.4142135623730951
```
