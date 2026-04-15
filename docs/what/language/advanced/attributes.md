---
title: "Attributes"
description: "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 30
slug: "attributes"
summary: "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language."
---

{{snippet:chapter-attributes}}

Attributes let you attach structured metadata to declarations.
They are not a replacement for ordinary language design, and they are not a free-form escape hatch for every feature.
They exist to carry information that matters at compile time, runtime boundaries, layout, tooling, or documentation.

## Boundary Tool

The built-in attribute families you will see most often are:

- compiler and foundation identity: `@known`, `@intrinsic`
- foreign boundary: `@link`, `@when`
- data layout and freezing: `@repr`, `@layout`, `@frozen`
- hotness and optimization hints: `@hot`, `@cold`
- lifecycle metadata: `@deprecated`, `@since`

There can also be non-reserved metadata attributes that survive as inert data for tooling or documentation.

## When to Reach for It

If the docs only say "attributes exist", users still do not know which ones are ordinary metadata, which ones affect code generation, and which ones are only valid in special places.
This chapter should answer three practical questions:

1. what family is this attribute in?
2. what kind of declaration can it attach to?
3. what does the compiler or runtime do with it?

## Read the Boundary

Read attributes from the outside in:

- the path, such as `@link` or `@layout`
- the named arguments, such as `name := "c"`
- the declaration the attribute is attached to

A short catalog of common meanings:

- `@known(name := "Bool")`: this exported item is one canonical built-in surface
- `@intrinsic(name := "ptr.load")`: implementation comes from compiler/runtime intrinsic machinery
- `@link(name := "c")`: foreign declaration links against host symbol provider
- `@when(...)`: gate declaration by target or environment facts
- `@repr(...)`, `@layout(...)`: influence data representation details
- `@frozen`: exported data layout should not drift casually
- `@hot`, `@cold`: codegen-facing temperature hint
- `@deprecated`, `@since`: consumer-facing lifecycle metadata

## What Musi does not do here

Musi attributes are not a full macro system.
They do not replace normal functions, data definitions, or effects.
If you need ordinary behavior, write ordinary Musi code first.
Reach for attributes when the information really is metadata.

## Small Exercise

- Read one `@link` declaration and identify every named argument.
- Compare one layout-related attribute with one lifecycle attribute.
- Ask whether the information belongs in ordinary code or in metadata.

## Mistake to Avoid

Do not treat attributes as a generic place to hide behavior.
If a concept changes how code runs, it usually deserves a language or library construct first.

## Next Page

Continue to [Foreign](/learn/book/advanced/foreign) to see how the FFI-related attributes fit into real declarations.
