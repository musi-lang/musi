---
title: "Attributes and foreign declarations"
description: "Use stable public attributes, reserved compiler attributes, and foreign bindings."
group: "Abstractions"
section: "Abstractions"
order: 13
slug: "attributes-and-foreign"
summary: "Stable public attrs, reserved compiler attrs, and foreign bindings."
---

Attributes are plain metadata on declarations. Most are public. Two are compiler-owned: `@known` and `@intrinsic`.

## Foreign binding

{{snippet:foreign-puts}}

## Public attributes

Use these when a declaration needs explicit metadata:

- `@link`
- `@when`
- `@repr`
- `@layout`
- `@frozen`
- `@hot`
- `@cold`
- `@deprecated`
- `@since`

{{snippet:attr-link-foreign}}

`@frozen` is ABI/layout promise for exported non-opaque `data`. It does not mean immutability.

`@hot` and `@cold` are optimizer hints on callable declarations. They do not change semantics.

## Reserved compiler attributes

Use reserved attrs only inside foundation/compiler-owned modules:

- `@known(name := "...")`
- `@intrinsic(name := "...")`

`@known` marks compiler-known bindings such as `Type` or `CString`.

`@intrinsic` marks compiler-owned runtime hooks in `musi:intrinsics`. It is not general user metadata.

## Try it

{{try:attributes-and-foreign}}

## Next step

Read foreign examples first, then continue to [Quote and syntax values](/docs/quote-and-syntax).
