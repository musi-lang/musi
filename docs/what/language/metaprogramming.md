# Metaprogramming

## Scope

Current metaprogramming surface includes:

- `quote`
- splice forms `#name`, `#(expr)`, `#[exprs]`
- syntax-valued expressions
- attributes as structured metadata

## Quote

`quote` captures syntax as syntax data.

It is part of the language surface and semantic model, not merely parser sugar.

Examples:

```musi
quote (x + 1);
quote {
  x;
};
```

## Splice

Splice forms are valid inside quote-shaped contexts.

They are syntax-directed and remain constrained by the surrounding quoted structure.

## First-Class Syntax

`Syntax` is a language-visible concept.

That matters for:

- sema typing
- compile-time metaprogramming boundaries
- runtime-host syntax-eval seams

## Boundaries

This document does not define:

- VM execution strategy for syntax values
- host compile/eval API details
- attribute behavior unrelated to syntax capture

## See Also

- `docs/what/language/compiler-attributes.md`
- `docs/what/runtime/seam-vm.md`
- `docs/why/runtime-boundary.md`
