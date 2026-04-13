# Syntax

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
- `export` and `export opaque`
- destructuring in bindings and imports

Top-level code is still expression-shaped even when it reads like declarations.

Examples:

```musi
export let twice (x : Int) : Int := x + x;
export opaque let Option[T] := data {
  | Some : T
  | None
};
```

## Naming Convention

Musi follows ECMAScript naming conventions by convention.

That means source examples and first-party libraries should prefer the usual ECMAScript shape:

- `camelCase` for values, functions, parameters, and record fields
- `PascalCase` for types and constructors
- `SCREAMING_SNAKE_CASE` for compile-time constants when they exist

This is documentation guidance, not compiler-enforced style.

## Records, Data, Effects, And Classes

Musi surface syntax includes:

- record literals, projection, and record update
- `data` declarations and variant constructors
- `effect` declarations with operations and laws
- `class` and `instance`
- `handle`, `perform`, and `resume`

Instance heads are target-first:

- `instance Foo { ... }`
- `instance Foo where Int : Mark { ... }`
- `instance[T] Eq[T] where T : Show { ... }`

These forms are language syntax, not tooling extensions.

## Control, Operators, And Literals

Surface control and value forms include:

- `;` sequences
- `case ... of` with guards
- symbolic infix operators
- integers, floats, strings, runes, and templates
- quote and splice forms

Effect handling uses `using` handlers:

```musi
handle perform Console.readln() using Console {
  value => value;
  readln(k) => resume "ok";
};
```

Operator precedence is part grammar, part semantic fixity handling; see the grammar reference for the compact formalization.

## Ranges

Musi uses a split range family:

- `Range[T]` for `left ..< right`
- `ClosedRange[T]` for `left .. right`
- `PartialRangeFrom[T]` for `left ..`
- `PartialRangeUpTo[T]` for `..< right`
- `PartialRangeThru[T]` for `.. right`

Range values expose these record-like fields:

- `Range[T]` and `ClosedRange[T]`: `lowerBound`, `upperBound`
- `PartialRangeFrom[T]`: `lowerBound`
- `PartialRangeUpTo[T]` and `PartialRangeThru[T]`: `upperBound`

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
- `grammar/MusiLexer.g4`
- `grammar/MusiParser.g4`
