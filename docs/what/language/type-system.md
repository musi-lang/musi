# Type System

## Core Principles

The type system is built around these defaults:

- types are first-class values
- `data`, `effect`, and `class` are expression-bound surface forms
- type application uses ordinary bracket application
- effects are tracked explicitly
- generic use is explicit rather than implicit HM-style generalization

## Core Type Space

Important built-in roles include:

- `Unit`
- `Int`
- `Float`
- `Bool`
- `String`
- `CString`
- `Module`
- `Syntax`
- `Type`
- `[]T`
- `[N]T`
- `Range[T]`
- `ClosedRange[T]`
- `PartialRangeFrom[T]`
- `PartialRangeUpTo[T]`
- `PartialRangeThru[T]`
- `mut T`

Tuples, records, sums, and function types are ordinary parts of the surface, not special compiler-only encodings.

Ranges use distinct type identities for open, closed, and one-sided forms. Musi exposes `Range[T]`, `ClosedRange[T]`, `PartialRangeFrom[T]`, `PartialRangeUpTo[T]`, and `PartialRangeThru[T]`.

## Type-Valued Expressions

Musi keeps one expression space, so type-valued forms stay visible at the language level:

- named types
- applied types
- function types
- tuples and records
- sum forms
- `forall`
- row-bearing function signatures

The compiler may lower these differently, but the surface stays uniform.

## Constraints And Rows

The language supports:

- `where` constraints
- class constraints
- subtype-style constraint surface where supported by sema
- open effect rows with `using { ... }`

Rows are part of function meaning, not late metadata.

Examples:

```musi
let port : Int := 8080;
let identityFn[T] (input : T) : T := input;
identityFn[Int](port);
```

## Data, Effects, And Classes

Type-facing declaration families include:

- product and sum data
- effect declarations and op signatures
- classes, members, instances, and laws

These constructs shape both language meaning and exported semantic surface.

## Mutability And Writable Locations

Names bind immutably by default.

Mutation happens through writable locations:

- `mut expr`
- writable arrays
- writable record fields
- assignment targets checked by sema

`mut T` is part of the type surface because writeability is semantic, not just operational.

## Boundaries

This document defines language-level type meaning.

It does not define:

- opcode shape
- VM storage layout
- bytecode metadata encoding

## See Also

- `docs/what/language/effect-system.md`
- `docs/what/language/ffi.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/reference/public-api.md`
