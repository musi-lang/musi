# Module And Native Boundaries

Status: proposed

Musi uses everyday boundary words:

| Other model    | Musi         |
| -------------- | ------------ |
| `use`          | `import`     |
| `extern`       | `native`     |
| `pub`          | `export`     |
| `#[link(...)]` | `@link(...)` |

## Exports

Top-level declarations are private by default. `export` adds a declaration to the module export record.

Exported and private bindings use the same binding semantics. `export` only changes module visibility.

`export (...)` is grouping sugar. It creates no value, no scope, and no runtime behavior.

The grouped form contains declarations; the group itself is not a runtime value.

This is equivalent to exporting each declaration separately.

Record fields are always public. Record fields have no private spelling and no visibility modifier.

## Imports

`import` brings in a Musi source/module export record.

A bound import is an ordinary binding whose value is the imported export record.

```musi
let io := import "std/io";
```

Musi does not use `as` for import aliases. `as` aliases matched/refined values only.

Rejected:

```musi
import "std/io" as io;
let io := import "std/io" as io;
```

Import blocks group imports.

An anonymous import group brings exported bindings into lexical scope according to resolver rules.

A let-bound import block produces a tuple of import records.

A let-bound import group produces a tuple of import records.

`import native` is not part of the language. Native boundaries use `native` directly.

## Native ABI

`native` declares a native ABI boundary. The optional string names the ABI and defaults to `"c"`.

```musi
@link(name := "c")
native "c" (
  let puts(s : CString) : CInt
);
```

A native block is grouping sugar for per-item native declarations. Block attributes are inherited by member declarations. Member attributes extend or override block metadata.

```musi
@link(name := "c")
native "c" (
  @link(symbol := "puts")
  let print(s : CString) : CInt
);
```

`export native` publishes a Musi declaration across a native ABI.

```musi
@link(name := "musi")
export native "c" let add(a : CInt, b : CInt) : CInt := a + b;
```

Rust 2024 comparison: this resembles an `extern "C"` boundary in host implementation code, but Musi keeps ABI exposure in attributes and native declarations instead of Rust syntax.

```rust
use core::ffi::c_int;

#[unsafe(no_mangle)]
pub extern "C" fn add(a: c_int, b: c_int) -> c_int {
    a + b
}
```

Source-facing boundary vocabulary excludes `use`, `extern`, `pub`, and `foreign`.
