# Contextual Shapes And Given Values

Status: proposed

This spec defines structural operation shapes and contextual provider values. The source model is first-class values plus contextual visibility, not typeclass declarations or hidden registries.

## Model

`shape` constructs a structural operation/contract shape value.

```musi
let Eq[T] := shape {
  ; let equal(a : T, b : T) : Bool
};
```

`given` constructs a provider value that is visible to contextual search when bound/imported in scope.

```musi
let intEq := given Eq[Int] {
  ; let equal(a : Int, b : Int) : Bool := a = b
};
```

A `given` parameter is an ordinary parameter with omission behavior.

```musi
let same[T](given eq : Eq[T], a : T, b : T) : Bool := (
  eq.equal(a, b);
);
```

## Binding Semantics

A given binding has the same value, type, exportability, importability, reflection, and lowering identity as a non-given binding. Extra consequence: contextual search can find it.

Exporting a given binding exports both ordinary binding visibility and contextual visibility. Importing it makes it visible only through normal import scope.

There is no hidden global instance registry in the source language.

## Parameter Semantics

- Explicit argument always wins.
- If omitted, contextual search must find exactly one matching visible `given` value.
- No match is a typed diagnostic.
- Multiple matches are a typed ambiguity diagnostic.

Search must be deterministic and explainable from lexical/imported/exported scope.

## Operators In Shapes

Operator members are first-class names.

```musi
let Add[T] := shape {
  ; let (+)(left : T, right : T) : T
};

let intAdd := given Add[Int] {
  ; let (+)(left : Int, right : Int) : Int := builtinAddInt(left, right)
};
```

`a + b` resolves to visible lexical operator bindings or contextual shape members. Operator definitions do not define precedence.

## Rust Comparison

Rust 2024 traits and impls are implementation reference material for dictionary passing, coherence pressure, specialization hazards, and monomorphization tradeoffs.

Musi does not expose Rust traits/impls as source model. The source consequences are `shape` values, `given` provider values, and `given` parameters.

Not source model terms:

- `class`
- `instance`
- `capability`
- `with`
- `via`
- `using`
- `for`
- `provide`

These words are ordinary identifiers unless later grammar gives them source meaning. They are not poison keywords and must not lower to the contextual model by compatibility magic.

## Lowering

The compiler may lower contextual parameters to explicit constraint-answer arguments, specialization, witness tables, or direct calls. Those are backend mechanics. They must not leak into source syntax or docs as the user model.
