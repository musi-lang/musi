# Known Items, Intrinsics, Attributes, Opaque, And FFI

Status: proposed

This spec defines final source-facing contract for compiler-owned known items, compiler-owned intrinsics, public attributes, `opaque`, and foreign bindings.

## Known Items

Musi does not use old lang-item role spelling in source.

Compiler-owned known bindings use reserved flat attribute:

```musi
@known(name := "Type")
export let Type := Type;
```

Rules:

- `@known` is valid only on exported plain-bind `let` in `musi:*`
- source names compiler-known item only by `name`
- compiler derives ownership and semantics from internal known-item registry
- ordinary library items such as `Option`, `Result`, `Rangeable`, `RangeBounds`, and `Abort` are not known items

Current known-item set includes core semantic and runtime-boundary names such as:

- `Type`
- `Array`
- `Any`
- `Unknown`
- `Syntax`
- `Empty`
- `Unit`
- `Bool`
- `Nat`
- `Int`
- `Float`
- `String`
- `Range`
- `ClosedRange`
- `PartialRangeFrom`
- `PartialRangeUpTo`
- `PartialRangeThru`
- `CString`
- `CPtr`

## Intrinsics

Compiler-owned lowering hooks use reserved flat attribute:

```musi
@intrinsic(name := "ptr.load")
foreign "c" let ptrLoad (ptr : CPtr) : Int;
```

Rules:

- `@intrinsic` is valid only on `foreign let` in `musi:intrinsics`
- intrinsic names are semantic hook names, not public opcode names
- intrinsics are reserved for irreducible runtime leaves such as pointer load/store/cast or boundary conversion
- source-facing APIs should wrap intrinsic-backed foreigns in ordinary Musi code

`@known` and `@intrinsic` are compile-time only. They do not survive as public reflection metadata.

## Public Attributes

Stable public attrs are flat:

- `@link(name := "...", symbol := "...")`
- `@when(os := "...", arch := "...", env := "...", abi := "...", vendor := "...", feature := ["..."])`
- `@repr(kind := "...")`
- `@layout(align := N, pack := N)`
- `@frozen`
- `@hot`
- `@cold`
- `@deprecated(message := "...", replace := "...", version := "...")`
- `@since(version := "...")`

Rules:

- `@repr` and `@layout` apply only to `data`
- `@frozen` applies only to exported non-opaque `data`
- `@hot` and `@cold` apply only to callable declarations
- `@hot` and `@cold` are mutually exclusive
- `@deprecated` and `@since` are public metadata
- `@link` and `@when` guide lowering and target gating

Reserved attribute space:

- `@known`
- `@intrinsic`
- `@musi.*`

## `@frozen`

`@frozen` is ABI/layout promise only.

It is not:

- immutability
- read-only aliasing
- constness

`@frozen` means exported non-opaque representation shape is stable. Changing field order, variant order, field count, variant count, `@repr`, or effective layout is breaking.

`@frozen` on opaque export is invalid.

## `@hot` And `@cold`

`@hot` and `@cold` are optimizer hints.

They do not change:

- type checking
- calling convention
- semantic behavior

Backends may ignore them. Profile data may override them.

## `opaque`

`opaque` is export-boundary abstraction only.

Rules:

- valid only on exported structural targets and structural aliases
- structural targets are `data`, `effect`, and `class`
- opaque imported data exposes nominal type only
- opaque imported effect exposes effect identity only
- opaque imported class exposes nominal constraint/type only
- opaque plain-value export is invalid

## FFI

Musi keeps explicit foreign syntax:

```musi
@link(name := "c", symbol := "puts")
foreign "c" let puts (msg : CString) : Int;
```

Rules:

- FFI uses `foreign "abi"`
- `@link` and `@when` are valid on foreign bindings
- `@hot` and `@cold` may annotate foreign bindings
- boundary pointer types are `CString` and `CPtr`
- typed unsafe raw pointer model belongs to `Ptr[T]`
- raw pointer operations require `unsafe (...)`
- safe borrows such as `Ref[T]`, `MutRef[T]`, and `Slice[T]` do not cross FFI directly

## Lowering

- `@known` and `@intrinsic` disappear before `.seam`
- `@frozen` lowers into type metadata and verification data
- `@hot` and `@cold` lower into method/foreign metadata flags
- `@deprecated` and `@since` lower into public metadata
- `@link` and `@when` lower into native/link decisions
