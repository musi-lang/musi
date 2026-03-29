# Type System Reference

Normative type-system reference for the reduced core language. Historical pre-reduction material lives in `docs/legacy/01-type-system.md`.

## Core Principles

- types are first-class values
- `data`, `effect`, and `class` are expressions bound through `let`
- type application uses brackets: `Option[Int]`
- functions are expression-valued and yield the value of their final expression
- effects are open from the start and are tracked through signature-side rows
- classes and instances are part of the language core
- `law` is class/effect specification syntax, not dispatch or precedence machinery

## Core Type Space

The language has these important built-in type roles:

| Type      | Role                     |
| --------- | ------------------------ |
| `Type`    | universe of types        |
| `Any`     | gradual/dynamic boundary |
| `Unknown` | imprecise top-like type  |
| `Empty`   | uninhabited type         |
| `Unit`    | singleton result type    |
| `Bool`    | truth values             |
| `Int`     | integer values           |
| `Float`   | floating-point values    |
| `String`  | text values              |

The precise runtime layout of these types belongs to the SEAM docs, not this document.

## Type Syntax

### Named And Applied Types

```musi
Int
Option[Int]
Result[Int, String]
State[Int]
```

There is no `Type of Arg` form in the current language.

### Functions

```musi
Int -> String
Int ~> String
```

- `->` is pure
- `~>` is effectful

Effect rows are not embedded directly inside arrow syntax in the current surface. They live on signatures through `with { ... }`.

### Products And Sums

```musi
(Int, String)
Int + String
```

- tuples are ordinary product types
- anonymous sums use `+`
- `|` is reserved for `data` variants and case/handler separators, not anonymous type sums

### Arrays

```musi
[]Int
[3]Int
[n]T
```

Arrays remain part of the type surface. Matrix syntax is not part of the language.

### Mutability

```musi
mut []Int
mut Point
```

`mut T` means writable `T`. Mutability belongs to the type/value model, not only to bindings.

## Type Definitions

All named type definitions are expressed through `let`.

### Product Types

```musi
let Point := data { x : Float; y : Float };
```

### Sum Types

```musi
let Option[T] := data { Some : T | None };
let Result[T, E] := data { Ok : T | Err : E };
```

### Effects

```musi
let State[S] := effect {
  let get () : S;
  let put (s : S) : Unit;
};
```

### Classes

```musi
let Eq[T] := class {
  let (=) (a : T, b : T) : Bool;
  law reflexive (x : T) := x = x;
};
```

## Type Parameters And Constraints

Type parameters are introduced with brackets.

```musi
let id[T] (x : T) : T := x;
```

Constraints stay in `where`.

```musi
let sort[T] (xs : []T) : []T where T : Ord := /* ... */;
let lift[T] (x : T) : Box[T] where T <: Value := /* ... */;
```

- `T : ClassName[...]` means class implementation
- `T <: Bound` means subtype-style bound

## Functions And Signatures

The current surface keeps effect rows on signatures.

```musi
let f (x : Int) : Int := x + 1;
let g (x : Int) with { Console } : Int := /* ... */;
let h[T] (x : T) with { State[T], ...r } : T := /* /* ... */;
```

That means:

- `->` and `~>` distinguish pure/effectful function kinds
- `with { /* ... */ }` states the effect row for a declaration/signature
- open effect rows use a named remainder such as `...r`

## Open Effect Rows

Effect rows are open by design.

Examples:

```musi
with { Console }
with { State[Int], Console }
with { State[Int], ...r }
```

Meaning:

- named effects state the known performed effects
- `...r` means “and the remaining effects carried through this abstraction”

This is not the same idea as “open classes” or “sealed classes” in OOP languages. It is row openness in the type/effect system.

## Classes, Instances, And Laws

Classes and instances are part of the language core.

```musi
let Ord[T] := class {
  let (<) (a : T, b : T) : Bool;
  let (<=) (a : T, b : T) : Bool;
  law reflexive (x : T) := x <= x;
};
```

```musi
instance Eq[Int] {
  let (=) (a : Int, b : Int) : Bool := int_eq(a, b);
};
```

`law` is:

- a named proposition over class or effect operations
- language-level specification syntax
- intended for docs, tooling, and generated property checks

`law` is not:

- precedence syntax
- dispatch machinery
- optimizer privilege

Derivation syntax is not part of the language core.

## Operators And Classes

Built-in arithmetic/comparison/word operators may be class-driven semantically, but precedence stays parser-owned.

- users can define symbolic operators
- users cannot define word operators
- precedence is not declared through the type system
- precedence is not controlled by `law`

## Purity And Effects

The type system distinguishes:

- pure computations
- effectful computations

The effect system document defines how `perform`, `handle`, and `resume` interact with these rows. This document only fixes the type surface:

- pure arrows stay pure
- effectful arrows participate in effect tracking
- effect rows are open and carried by named remainders

## What Is Out Of Scope For The Type Core

The reduced language does not include:

- `return`
- derivation syntax
- comprehensions
- matrix literals
- ranges
- piecewise conditionals

Those forms do not belong in the current type surface and should not appear in canonical examples.
