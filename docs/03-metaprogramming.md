# Metaprogramming Reference

Current metaprogramming surface for the language.

## Scope

The core language keeps:

- `quote`
- splice forms `#name`, `#(expr)`, `#[exprs]`
- attributes as structured metadata and control surface

The current metaprogramming surface is limited to the constructs above.

## Quote

`quote` captures syntax as compile-time data.

```musi
quote (x + 1)
quote { let f = 42; }
```

Grammar:

```abnf
ast-quote = "quote" ("(" ast-expr ")" / "{" *(ast-stmt) "}")
```

`quote` is the language surface for syntax values. It is not a macro declaration system.

## Splice

Splice inserts quoted syntax back into a `quote`.

```musi
let name = quote x;
let expr = quote (#name + 1);
let stmts = [quote (let a = 1), quote (let b = 2)];
let block = quote { #[stmts]; };
```

Grammar:

```abnf
splice = "#" ident
       / "#(" ast-expr ")"
       / "#[" expr-list "]"
```

Splice forms are only valid inside quoted syntax.

## Attributes

Attributes remain part of metaprogramming, but they are not all the same kind of thing.

```musi
@link(name = "c", symbol = "puts")
foreign let puts (s : CString) : Int;

@diag.allow("ms4023")
let x = 1;
```

The language splits attrs into three tiers:

- public language attrs such as `@link`, `@when`, `@repr`, `@layout`, and `@diag.*`
- compiler-only `@musi.*` such as `@musi.lang`, `@musi.intrinsic`, `@musi.variant_tag`, `@musi.runtime_layout`, and `@musi.codegen`
- inert metadata that tooling may read but the compiler does not give semantic privilege

Attributes are not a substitute for macros or derivation syntax.

## Current Boundaries

Metaprogramming in the current core means:

- syntax capture
- syntax splicing
- metadata annotation

It does not mean:

- general runtime reflection
- procedural macros
- compile-time proof search
- automatic instance derivation

## First-Class Syntax

`quote` exists because syntax values are part of the compile-time language model.

- syntax can be captured
- syntax can be passed into compile-time helpers
- syntax can be reinserted with splices

That is the only first-class metaprogramming surface in the current core.
