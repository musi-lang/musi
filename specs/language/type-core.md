# Core Type Model

Status: proposed

## Universe Type

`Type` is the source-facing name for ordinary types.

Universe levels are a compiler typing model, not everyday source syntax. They should stay hidden unless source code needs explicit dependent-universe precision.

## Top, Unknown, Unit, And Bottom

`Unknown` is an inference/error-hole type. It means the compiler does not know the precise type yet, or typing already recovered from an error. It is not the safe top type.

`Any` is the dynamic/top value type. Values of type `Any` require checks, casts, or pattern refinement before precise operations.

`Unit` has exactly one value. It is used for expressions with no meaningful result.

`Empty` is the uninhabited bottom type. It represents impossible results, divergence, and unreachable/exhaustive cases.

## Product, Sum, Tuple, And Sequence Shapes

Delimiter shape matters in type position.

- `(T1, T2)` is the product type form.
- `T1 + T2` is the sum type form.
- `A -> B` is the pure callable type form.
- `A ~> B` is the effectful callable type form.
- `()` is `Unit`.
- `(,)` is the empty tuple.
- `(;)` is the empty sequence.

## Mutability Reading Model

Binders name things. Types describe accepted capability. Values construct capability.

`mut` belongs in type and value positions, not binder positions.

```musi
let cell := mut value;
let write(cell : mut Int) : Unit := cell := 2;

data {
  slot : mut Int;
};

let point := { slot := mut 1 };
```

Rejected source forms:

```musi
let mut cell := value;
let write(mut cell : Int) : Unit := cell := 2;
data { mut slot : Int; };
```

This avoids modifier soup with `let rec` and keeps parameters, fields, and let-bindings read the same way: the name is the binder, while `mut` describes the type or constructed value.

## Optional And Fallible Types

`Option` and `Result` are ordinary library types. The source language gives them short type sugar because absence and failure are common semantic consequences.

- `?T` means `Option[T]`.
- `E!T` means `Result[T, E]`.

Bare `!T` is not core syntax. It may be introduced only if the standard library defines a default error type and the spec defines `!T` as an alias for `Error!T`.

Postfix expression `?` and `!` are not core syntax. Force unwrap is a named library operation because it can fail at runtime.

## Optional And Fallible Expressions

`??` is optional fallback.

- left operand has type `?T`
- right operand has type `T`
- result has type `T`
- the right operand is used only when the left operand is absent

`catch` is fallible recovery.

- left operand has type `E!T`
- recovery operand may map `E` to `T`
- recovery operand may map `E` to `F!T`
- result is `T` or `F!T`, respectively

`catch` handles failure values. It is not an exception block and does not introduce stack unwinding as source semantics.

## Optional And Fallible Chains

`?.` and `!.` are compound access operators. They belong to the access edge, not to the identifier.

Supported selector forms:

```musi
x?.name
x?.[i]
x?.(+)
x!.name
x!.[i]
x!.(+)
```

Optional access:

- base has type `?T`
- absent base short-circuits to absent result
- present base accesses the selected member/index/call
- result stays flattened as optional

Fallible access:

- base has type `E!T`
- failed base short-circuits to failed result
- successful base accesses the selected member/index/call
- result stays fallible

Mixed optional and fallible edges in one chain require an explicit bridge with `??`, `catch`, or a named conversion. The compiler must not choose a nested shape implicitly.

## Type Tests And Refinement Aliases

`:?` tests whether a value conforms to a type.

```musi
value :? T;
value :? T as refined;
```

`as` in a type test binds the refined view/value. This matches pattern `as`: both spellings mean “bind this matched/refined value as this name.”

`as` is not alias syntax for imports, exports, modules, or types.

## Known Values

`known` is a source keyword for compile-time availability.

- A `known` binding is available to compile-time elaboration/evaluation.
- A `known` parameter needs a compile-time-known argument.
- A known function is an ordinary function value whose binding is compile-time available.

`known` does not mean contextual. `given` owns contextual search.

`known` is distinct from `@musi.builtin`. The keyword describes user-authored compile-time availability. The attribute marks compiler-owned builtin items in foundation modules.

## Proof Evidence

`Proof[P]` is the first-class evidence type for proposition `P`.

```musi
law reflexive[T](x : T) := x = x;

let p : Proof[x = x] := known;
```

`Proof[P]` is an ordinary type expression. It does not need special parser syntax. The type checker and resolver decide whether a witness can be constructed, found through `given`, imported, or supplied by a named law or `@axiom` binding.

`law` declares an obligation/property. `@axiom` marks a bodyless proof binding as a trusted root that produces `Proof[P]`.

```musi
@axiom(reason := "trusted proof principle")
let extensionality[A, B](f : A -> B, g : A -> B)
  : Proof[((x : A) -> f(x) = g(x)) -> f = g];
```

`@axiom` is auditable trust metadata attached to a normal binding. Consumers see a first-class value/function of the declared type.
