# First-Class Everything

Status: proposed

This spec locks Musi's first-class design. Rust 2024 edition with `rust-version = "1.87.0"` remains the host implementation baseline, but Musi source syntax follows user-visible consequences, not host mechanics.

Core rule: a concept is first-class when it can be bound, appear as an expression result, be passed, returned, stored where phase/lifetime permits, imported/exported under visibility, and composed as an expression terminated with `;` in sequence/top-level position.

`let` names values. `law` names obligations. `@axiom` marks trusted proof roots as metadata on bodyless proof bindings. Other form words construct values or alter expression consequences; they do not introduce hidden declaration naming positions.

## Comparison Policy

Use comparison languages to identify consequences, not copy syntax.

- Function reference: F# for typed expression-oriented functions.
- Control reference: Scheme for first-class continuations/control.
- Dependent type reference: Idris for programming-facing first-class types.
- Data-type reference: Generic Haskell for generic structure over data shapes.
- Polymorphism reference: Musi should fill this gap directly.
- Message reference: Common Lisp for operation/message values separated from receiver syntax.
- Class/metaobject reference: Smalltalk and Common Lisp CLOS for first-class metaobject consequences.
- Proof reference: Agda for proofs-as-programs and explicit proof values.

## First-Class Functions

Functions are ordinary values. Anonymous functions must start with `\`.

```musi
let add : (Int, Int) -> Int :=
  \(a : Int, b : Int) : Int => (
    a + b;
  );

let apply(f : (Int, Int) -> Int, a : Int, b : Int) : Int := (
  f(a, b);
);
```

`->` denotes pure function type. `~>` denotes effectful function type.

## First-Class Control

Control is represented by effect requests, answer values, handling, and continuation use.

```musi
let Console := effect {
  ; let readLine() : String
};

let consoleAnswer := answer Console {
  ; let readLine() := (
      resume("input");
    )
};

let read : () ~> String := \() : String => (
  ask Console.readLine();
);

let readPure : () -> String := \() : String => (
  handle (read();) answer consoleAnswer;
);
```

`ask` creates an effect requirement. `handle ... answer ...` applies an answer value and eliminates or translates that requirement.

## First-Class Types

Types are values at type/known phase where the type system permits them.

```musi
let Box[T] := data {
  ; let value : T
};

let IntBox := Box[Int];
```

Runtime type/meta reflection must be explicit, such as through `meta(Box[Int])`, not automatic.

## First-Class Data Types

`data` constructs a data shape value. Record members are `let` entries; variants use `|` and dot access.

```musi
let Maybe[T] := data {
  | Some(T)
  | None
};

let Pair[A, B] := data {
  ; let left : A
  ; let right : B
};

let value := .Some(1);
```

## First-Class Polymorphism

Polymorphic values remain values before specialization.

```musi
let id[T](value : T) : T := (
  value;
);

let poly := id;
let intId := poly[Int];
intId(1);
```

`[T]` type application is expression syntax. Musi defines first-class polymorphism directly rather than inheriting a declaration-only model.

## First-Class Messages And Operators

Message/operator selection can produce values.

```musi
let read := Console.readLine;
let plus := Add.(+);
let maybePlus := maybeAdd?.(+);
let fallibleIndex := fallibleArray!.[i];
```

Operators are first-class names with infix/prefix surface sugar. `(+)` is the operator value; `Add.(+)` selects an operator-named member.

## First-Class Metaobjects

`shape`, `data`, and `effect` construct shape/metaobject-capable values without introducing `class` syntax.

```musi
let Eq[T] := shape {
  ; let equal(a : T, b : T) : Bool
};

let eqMeta := meta(Eq[Int]);
```

Reflection stays explicit and bounded by visibility.

## First-Class Proofs

`law` declares a named obligation. `Proof[P]` is the first-class evidence type for proposition `P`. `@axiom` marks a bodyless proof binding as a named trusted proof root when Musi must accept evidence without deriving it.

```musi
let Eq[T] := shape {
  ; let equal(a : T, b : T) : Bool

  ; law reflexive(x : T) := (
      (equal(x, x)) = .True;
    )
};

@axiom(reason := "trusted external theorem")
let sortedAfterSort[T](xs : List[T])
  : Proof[sorted(sort(xs)) = .True];

let proofValue : Proof[sorted(sort(xs)) = .True] := sortedAfterSort(xs);
```

Proof values bind/pass/import/export like ordinary values. `given` can publish proof evidence contextually. `known` can resolve proof evidence from expected type where contextual search permits it. Proof values are erased by default unless explicitly reflected.

`@axiom` is not an expression and not a proof-assistant tactic. It is metadata on an ordinary binding. The binding stays first-class; the attribute makes the trust root auditable. Package policy may reject modules that depend on axioms or require an allow-list.

There is no `proof (...)` keyword. The target type `Proof[P]` states the consequence; functions, laws, givens, known search, or `@axiom` bindings provide the witness.

## Open Design Checks

Before adding syntax, check first-class status:

- Can this concept be bound or named by its consequence-bearing declaration form?
- Can it appear as expression result?
- Can it be passed to a function?
- Can it be stored if phase/lifetime permits storage?
- Can it cross module boundaries under ordinary visibility?
- Are mechanics hidden while consequences are exposed?

If answer is no, the concept should not receive privileged source syntax.
