# Core Type Model

Status: frozen 0.1.0 host-language baseline

## Primitive And Library Types

`Bit` is the primitive condition type. Conditions require `Bit`; there is no
truthiness for integers, arrays, strings, unit, or data values.

`Unknown` is the opaque top type. Any value can become `Unknown`, but useful
operations require narrowing. `Unit` has exactly one value and is written `()`.
`Empty` is uninhabited.

`Type` is the type of type-phase type expressions. It is not a runtime type
object.

`Any` is the dynamic type. Dynamic operations on `Any` are runtime checked.

## Type Boundaries And Conformance

Conformance uses `|=`:

```musi
T |= Shape
```

Read this as "`T` conforms to `Shape`".

The word `fits` is reserved for diagnostics and tooling prose, but it is not
source syntax.

Type equivalence uses `~=`:

```musi
A ~= B
```

Static or guaranteed casts use `:>`:

```musi
let a : Any := value :> Any;
```

Runtime type tests use `:?>`:

```musi
let isInt : Bit := value :?> Int;
```

Final source boundary forms:

```text
|=
~=
:>
:?>
```

Internal compiler prose may write subtype relations mathematically. Musi source
uses the boundary forms above.

## Data Shapes

`data` defines concrete data. Product fields use `let` members with semicolon
terminators:

```musi
let Buffer := data {
  let ptr : Ptr[mut Byte];
  let len : Nat;
};
```

Sum variants use `|` alternatives without semicolons:

```musi
let Maybe[T] := data {
| Some(value : T)
| None
};
```

Do not mix product fields and sum variants in one `data` body.

## Mutability

`mut` is local to the type or value on its right.

```musi
let x := mut 0;
let x : mut Int := mut 0;
let p : Ptr[mut Byte] := getMutablePtr();
let q : mut Ptr[mut Byte] := mut getMutablePtr();
```

An explicit mutable annotation does not make an immutable initializer mutable.

## Absence And Failure

Absence and failure are algebraic data:

```musi
?T   == Maybe[T]
E!T  == Expect[T, E]
```

`??` is Maybe fallback only. Failure is handled with `match`, `let ... else`,
or named helpers.

## Callable Types

`A -> B` is a callable type. Declarations use `:` for result types.

```musi
let add(a : Int, b : Int) : Int := a + b;
let f : Int -> Int := \(x : Int) => x;
```

## Ranges

Range expressions are values:

```musi
a ..< b   -- half-open [a, b)
a .. b    -- inclusive [a, b]
```

`...` remains spread/splat syntax and is lexed before range tokens.
