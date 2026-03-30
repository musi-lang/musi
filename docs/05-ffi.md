# FFI Reference

Musi keeps FFI split between syntax and attrs:

- `foreign "abi"` marks an ABI-facing declaration
- `@link(...)` supplies native library and symbol metadata
- `@when(...)` gates declarations by target
- `@repr(...)` and `@layout(...)` mark layout-sensitive foreign-facing data

If the ABI string is omitted, the default ABI is `"c"`.

## Boundary Model

The intended FFI surface is an explicit C-shaped boundary.

Direct FFI signatures use:

- integer scalars
- floating scalars
- `Bool`
- `Unit`
- `CString`
- `CPtr`
- aggregates explicitly marked with `@repr(...)` and optional `@layout(...)`

Plain Musi `String` is not part of the direct FFI contract. Convert at the boundary with explicit library helpers or binding wrappers.

## Inbound Foreign Declarations

Single declaration:

```musi
@link(name := "c", symbol := "puts")
foreign "c" let puts (msg : CString) : Int;
```

Grouped declarations:

```musi
@link(name := "m")
foreign "c" (
  let sin (x : Float) : Float;
  let cos (x : Float) : Float;
);
```

Attrs before a grouped `foreign (...)` block are copied onto each declaration inside the block.

## `@link`

`@link(name, symbol)` accepts named or positional arguments.

```musi
@link("c", "puts")
foreign "c" let puts (msg : CString) : Int;
```

- `name`: native library or link target
- `symbol`: native symbol override

If `symbol` is omitted, the Musi binding name is used.

If `name` is omitted, lookup falls back to compiler-owned builtin host symbols.

## `@when`

`@when(os, arch, env, abi, vendor, feature...)` conditionally includes a foreign declaration for a target.

```musi
@when(os := "linux", arch := "x86_64")
foreign let clock_gettime (id : Int, out : CPtr) : Int;
```

To express multiple alternatives, use arrays:

```musi
@when(os := ["linux", "mac"], arch := ["x86_64", "aarch64"])
foreign let clock_gettime (id : Int, out : CPtr) : Int;
```

Use named predicates as the canonical form.

`@when` is target selection, not general compile-time branching.

## Builtin Host Symbols

Compiler-owned builtin host symbols do not require `@link(name := ...)`:

```musi
foreign let musi_io_read_text (path : CString) : CString;
foreign let musi_io_write_text (path : CString, contents : CString) : Bool;
```

## Outbound `export foreign`

Outbound host-callable exports are part of the intended FFI surface and use the same ABI and attr model as inbound declarations:

```musi
export foreign "c" let my_callback (x : Int) : Int := x * 2;
```

## Aggregate Interop

Aggregate interop is explicit.

Only declarations marked with `@repr(...)` and, when needed, `@layout(...)` are part of the direct aggregate FFI surface:

```musi
@repr(kind := "c")
@layout(align := 8, pack := 1)
let Header := data { tag : Int; len : Int };
```

Ordinary `data` declarations do not become FFI-safe by accident.

## ABI Strings

The ABI surface is an open string on the declaration:

```musi
foreign "c" let puts (msg : CString) : Int;
foreign "stdcall" let callback (x : Int) : Int;
```

Canonical known names include:

- omitted: `"c"`
- `"c"`
- `"cdecl"`
- `"stdcall"`
- `"fastcall"`

This follows the same broad direction as Rust and Odin: the ABI is explicit at the declaration site, but the language surface is not frozen to one closed list forever.
