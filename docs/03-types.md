# §3 — Types

## 3.1 Types Are First-Class Values

Types have kind `Type`. `Int`, `String`, `Bool` etc. are stdlib identifiers — not keywords. They live in the same namespace as values. No separate type namespace exists.

Special identifiers: `Any` (⊤), `Never` (⊥), `Type` (kind of all types).

## 3.2 Primitives

| Type | Description |
|------|-------------|
| `Int` | Platform-width signed integer |
| `Int8` `Int16` `Int32` `Int64` | Explicit-width signed |
| `UInt8` `UInt16` `UInt32` `UInt64` | Explicit-width unsigned |
| `Float32` `Float64` | IEEE 754 |
| `Rune` | Unicode scalar value |
| `String` | `Array of Rune` |
| `Bytes` | `Array of UInt8` |
| `Bool` | `True + False` |
| `()` | Unit — sugar for `Unit` |
| `Never` | Bottom type ⊥ — uninhabited |

`?'T` is sugar for `Option of 'T`. Not a nullable pointer.

## 3.3 Record Types

Fields separated by **semicolons**. Fields may carry default values.

```musi
let Point  := { x: Int; y: Int };
let Config := { host: String; port: Int := 8080; debug: Bool := False };
```

## 3.4 Record Literals (UDN)

Leading dot, fields separated by **commas**, initialised with `:=`.

```musi
let p   := Point.{ x := 1, y := 2 };      // named
let p   := .{ x := 1, y := 2 };           // anonymous — type inferred
let cfg := Config.{ host := "localhost" }; // omits defaulted fields
```

> **Type vs literal syntax is intentionally distinct.**
> Type: `{ x: Int; y: Int }` — colons, semicolons.
> Literal: `.{ x := 1, y := 2 }` — `:=`, commas, leading dot.

## 3.5 Parenthesised Types

| Form  | Meaning |
|-------|---------|
| `()`  | Unit type/value |
| `(,)` | Empty tuple |
| `(A * B)` | Tuple |
| `(A, B)` | Tuple (comma = product separator in type position) |

## 3.6 Sum Types

```musi
let Shape  := Circle + Rect;
let Option := Some of 'T + None;
let Result := Ok of 'T + Err of 'E;
```

Variant construction and destruction use UDN dot-prefix:

```musi
.Some(5)    .None    .Ok(v)    .Err(e)      // construct
```

Sum type literal binding requires type annotation or inference:

```musi
let x : Option of Int := .Some(5);
let y : Option of Int := .None;
```

## 3.7 Refinement Types

```musi
let Positive    := { n: Int | n > 0 };
let Probability := { f: Float64 | f >= 0.0 and f <= 1.0 };
let NonEmpty    := forall 'T -> { xs: Array of 'T | xs.length > 0 };
```

| Binding | Enforcement |
|---------|-------------|
| `let`   | Static — compile-time discharge |
| `var` / `ref` | Dynamic — compiler inserts runtime assertion, triggers `Throw.Raise` on violation |

## 3.8 Parameterised Types

`over` declares, `of` instantiates. No angle brackets.

```musi
let xs : List of Int        := ...;
let m  : Map of String, Int := ...;
```

## 3.9 Quantification

```musi
let id   := forall 'T -> (x: 'T) -> 'T -> x;
let sort := forall 'T where 'T <: Ord -> Array of 'T -> Array of 'T;
let mkCounter : exists 'S -> { state: 'S; inc: 'S -> 'S };
```

## 3.10 Subtyping

Structural. No declaration required.

```musi
let Animal := { name: String };
let Dog    := { name: String; breed: String };
// Dog <: Animal automatically

let a : Animal := Dog.{ name := "Rex", breed := "Lab" }; // valid
```

`<:` and `:>` appear only in constraint position — never as value-level operators.

## 3.11 Row Polymorphism

```musi
let greet := (x: { name: String; ... }) -> f"hello {x.name}";
```

`...` = "and any additional fields."

## 3.12 Variance

| Position | Variance |
|----------|----------|
| Function input | Contravariant |
| Function output | Covariant |
| Mutable field | Invariant |

## 3.13 Coercion

No implicit coercion. All explicit via `Into`:

```musi
let n : Float64 := 5.into();
let s : String  := 42.into();
```

## 3.14 Heap Allocation

Values are stack-allocated and copied by default. `ref` allocates on the heap with shared identity, always mutable through:

```musi
let p := ref Point.{ x := 1, y := 2 };
```

See §5 for full memory semantics.
