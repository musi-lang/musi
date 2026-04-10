# Syntax

**What**: front-door reference for Musi surface syntax.
**Why**: defines the visible language shape before semantic meaning is applied.
**How**: use this when reading or writing source, parser rules, or CST-facing tests.
**Where**: canonical grammar lives in `grammar/Musi.g4` and `grammar/Musi.abnf`.

## Core Shape

Musi is expression-driven:

- functions yield the value of the final expression
- top-level evaluation is the startup model
- typed positions reuse ordinary expression syntax
- control flow is built from sequences, `case`, and handlers

The language does not use statement-only control constructs as the primary model.

## Names, Bindings, And Modules

Core surface forms:

- `let` and `let rec`
- lambdas and named callables
- `import "..."` and `import expr`
- `export` and `opaque export`
- destructuring in bindings and imports

Top-level code is still expression-shaped even when it reads like declarations.

## Records, Data, Effects, And Classes

Musi surface syntax includes:

- record literals, projection, and record update
- `data` declarations and variant constructors
- `effect` declarations with operations and laws
- `class` and `instance`
- `handle`, `perform`, and `resume`

These forms are language syntax, not tooling extensions.

## Control, Operators, And Literals

Surface control and value forms include:

- `;` sequences
- `case ... of` with guards
- symbolic infix operators
- integers, floats, strings, runes, and templates
- quote and splice forms

Operator precedence is part grammar, part semantic fixity handling; see the grammar reference for the compact formalization.

## Foreign And Attribute Surface

The syntax includes:

- `foreign`
- `@link(...)`
- `@when(...)`
- `@repr(...)`
- `@layout(...)`
- compiler-reserved `@musi.*`

Attributes remain explicit metadata and control forms. They do not replace ordinary language semantics.

## Boundaries

This document defines surface shape only.

It does not define:

- type rules
- effect rules
- runtime behavior
- crate ownership

## See Also

- `docs/what/language/type-system.md`
- `docs/what/language/effect-system.md`
- `docs/reference/grammar-ebnf.md`
- `grammar/Musi.g4`
