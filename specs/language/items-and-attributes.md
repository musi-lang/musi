# Builtin Items, Intrinsics, And Attributes

Status: frozen 0.1.0 host-language baseline

`known` is source syntax for compiler-known phase information. Reserved
`@musi.*` attributes mark compiler-owned foundation bindings and intrinsics.

## Builtin Items

Compiler-owned builtin bindings use reserved namespaced metadata:

```musi
@musi.builtin(name := "Type")
export let Type := Type;
```

Rules:

- `@musi.builtin` is valid only on exported foundation bindings.
- The source name is tied to the internal builtin registry by `name`.
- Library items such as `Maybe`, `Expect`, and helpers remain ordinary
  library values unless the compiler registry owns them.

## Intrinsics

Irreducible runtime leaves use reserved intrinsic metadata:

```musi
@musi.intrinsic(name := "ptr.load")
let ptrLoad(ptr : CPtr) : Int;
```

Source-facing APIs should wrap intrinsic-backed leaves in ordinary Musi code.

## Public Attributes

Accepted public source attributes:

```text
@deprecated
@skip
@layout
@foreign
@link
@target
```

Attribute names and keys use camelCase where needed.

## `@deprecated`

Lifecycle metadata for APIs and tooling.

```musi
@deprecated(
  use := "readFile",
  note := "renamed"
)
let readAll := ...;
```

## `@skip`

Explicit skipped compiler or verifier checks.

```musi
@skip(
  checks := [.bounds],
  why := "caller already checks index"
)
```

## `@layout`

Memory or storage representation metadata.

```musi
@layout(
  form := .packed,
  align := 1
)
let Header := data {
  let tag : Byte;
  let size : Word;
};
```

Common forms:

```text
.packed
.c
.transparent
```

## `@foreign`

Foreign ABI linkage uses `@foreign` on callable declarations. Direction is derived
from `export` and body presence.

Rules:

- `@foreign` plus a declaration without a body imports an external implementation.
- `@foreign` plus `export` plus a body exports an external entry point.
- `export` without `@foreign` is a public Musi API only.
- `@foreign` without `export` but with a body is invalid.
- Do not duplicate direction with `mode`, `import`, or `export` keys.

The attribute may include external facts such as ABI and foreign symbol name.

```musi
@foreign(abi := .c, symbol := "strlen")
export let strlen (value : CString) : Nat;
```

## `@link`

Declares a native library/link requirement for a foreign binding.

```musi
@link(name := "m")
@foreign(abi := .c, symbol := "pow")
let pow (base : Float, exponent : Float) : Float;
```

## `@target`

Target/platform availability predicate.

```musi
@target(os := "linux", arch := "x86_64", pointerWidth := 64)
let platformWordSize := 64;
```
