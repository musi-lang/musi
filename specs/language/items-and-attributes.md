# Builtin Items, Intrinsics, And Attributes

Status: proposed

This spec defines final source-facing contract for compiler-owned builtin items, compiler-owned intrinsics, and public attributes.

`known` as a source keyword is documented in `specs/language/type-core.md`. It describes compile-time availability for user-authored bindings and parameters.

`@musi.builtin` is different. It marks compiler-owned foundation names. It is not source spelling for compile-time values.

## Builtin Items

Musi does not use old lang-item role spelling in source.

Compiler-owned builtin bindings use reserved namespaced attribute:

```musi
@musi.builtin(name := "Type")
export let Type := Type;
```

Rules:

- `@musi.builtin` is valid only on exported plain-bind `let` in `musi:*`
- source names compiler builtin only by `name`
- compiler derives ownership and semantics from internal builtin registry
- ordinary library items such as `Option`, `Result`, `Rangeable`, `RangeBounds`, and `Abort` are not builtin items
- optional and fallible sugar (`?T`, `E!T`) lowers through ordinary `Option` and `Result` types, not builtin hooks

Current builtin item set includes core semantic and runtime-boundary names such as:

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

Compiler-owned lowering hooks use reserved namespaced attribute:

```musi
@musi.intrinsic(name := "ptr.load")
let ptrLoad (ptr : CPtr) : Int;
```

Rules:

- `@musi.intrinsic` is valid only on plain-bind `let` in `musi:intrinsics`
- intrinsic names are semantic hook names, not public opcode names
- intrinsics are reserved for irreducible runtime leaves such as pointer load/store/cast or boundary conversion
- source-facing APIs should wrap intrinsic-backed leaves in ordinary Musi code

`@musi.builtin` and `@musi.intrinsic` are compile-time only. They do not survive as public reflection metadata.

## Public Attributes

Stable public attrs are behavior-first and short:

- `@link(name := "...", symbol := "...")`
- `@target(os := "...", arch := "...", env := "...", abi := "...", vendor := "...", feature := ["..."])`
- `@repr(kind := "...")`
- `@layout(align := N, pack := N)`
- `@frozen`
- `@profile(level := .hot)`
- `@profile(level := .cold)`
- `@lifecycle(since := "...", deprecated := { message := "...", replace := "...", version := "..." })`
- `@axiom(reason := "...")`

Rules:

- `@repr` and `@layout` apply only to `data`
- `@frozen` applies only to exported `data`
- `@profile` applies only to callable declarations
- `@profile` accepts only `level := .hot` or `level := .cold`
- `@lifecycle` carries public lifecycle metadata
- `@link` and `@target` guide lowering and target gating
- `@axiom` applies only to bodyless `let` bindings whose declared type is `Proof[P]` or a function returning `Proof[P]`

Reserved attribute space:

- `@musi.builtin`
- `@musi.intrinsic`
- `@musi.*`

## `@frozen`

`@frozen` is ABI/layout promise only.

It is not:

- immutability
- read-only aliasing
- constness

`@frozen` means exported representation shape is stable. Changing field order, variant order, field count, variant count, `@repr`, or effective layout is breaking.

## `@profile`

`@profile(level := .hot)` and `@profile(level := .cold)` are optimizer hint metadata.

They do not change:

- type checking
- calling convention
- semantic behavior

Backends may ignore them. Profile data may override them.

## `@axiom`

`@axiom` marks a trusted proof root.

```musi
@axiom(reason := "trusted external theorem")
let sortedAfterSort[T](xs : List[T])
  : Proof[sorted(sort(xs)) = .True];
```

Rules:

- `@axiom` permits a bodyless `let` proof binding
- `@axiom` does not create a separate keyword or expression form
- axiom binding is imported, exported, passed, and stored like any other first-class value
- trust metadata remains available to audit/reporting/package policy
- optional `reason` explains the trusted source
- anonymous axiom expressions are not source syntax

`@axiom` changes trust classification, not call semantics. Consumers see only the declared value/function type.

## Native Boundary Metadata

Native linkage uses `native` declarations plus attributes. `import native` is not source syntax.

Rules:

- `native "abi"` declares ABI boundary; omitted ABI defaults to `"c"`
- `@link` and `@target` annotate native `let` declarations
- `@link(name := "...")` names link/library input
- `@link(symbol := "...")` names native symbol override
- `@profile` may annotate native callables
- boundary pointer types are `CString` and `CPtr`
- typed unsafe raw pointer model belongs to `Ptr[T]`
- raw pointer operations require `unsafe (...)`
- safe borrows such as `Ref[T]`, `MutRef[T]`, and `Slice[T]` do not cross boundary calls directly

## Lowering

- `@musi.builtin` and `@musi.intrinsic` disappear before `.seam`
- `@frozen` lowers into type metadata and verification data
- `@profile` lowers into callable metadata flags
- `@lifecycle` lowers into public metadata
- `@link` and `@target` lower into native/link decisions
- `@axiom` lowers into proof-trust metadata and may be rejected by package policy
