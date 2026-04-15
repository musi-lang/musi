---
title: "Generics"
description: "Introduce type parameters after annotations and inference make sense."
group: "Types"
section: "Types"
order: 20
slug: "generics"
summary: "Write reusable functions over many types without losing clarity."
---

{{snippet:chapter-generics}}

## In this chapter

Generics let one definition work across many concrete types by abstracting over a type parameter such as `T`.
This page keeps the example deliberately small: identity function in generic form, then one explicit application at `Int`.
That is enough to teach the core move without turning the chapter into a full type-system reference.

## Why it matters

Sooner or later users ask how to avoid copy-pasting same function for several types.
Generics answer that need, but they become overwhelming when introduced before annotations and inference are stable.
Here the goal is practical reuse: one function shape, many compatible inputs.

## Walk through it

Read `[T]` as parameterization over a type, not over a runtime value.
Then trace where `T` appears in input and output positions to understand what stays same across all uses.
At the call site, `identityFn[Int](port)` makes type application explicit; that can be useful whenever you want the chosen type to be obvious to reader.

Generic functions are still values. You can put one in a record, pass it through another binding, then type-apply it later:

```text
let identityFn[T] (input : T) : T := input;
let tools := { identity := identityFn };

tools.identity[Int](8080);
```

That matters because Musi does not treat generic functions as a separate second-class feature. The name, the alias, and the record field all carry the same polymorphic value.

## Type constructors as parameters

A normal type parameter stands for one concrete type:

```text
let identityFn[T] (input : T) : T := input;
```

A higher-kinded parameter stands for a type constructor. Write its kind with type arrows:

```text
F : Type -> Type
```

That says `F` needs one type argument before it becomes a concrete type. `Option` fits that shape because `Option[Int]` is concrete. A two-argument constructor can be partially applied from the left, so `Box2[String]` also fits `Type -> Type`.


Use this when an abstraction cares about the outer shape. `Functor[Option]` talks about mapping inside `Option`; `Functor[Box2[String]]` talks about mapping inside a two-argument constructor after the first argument has already been chosen.

Musi uses `F : Type -> Type` instead of placeholder syntax such as `F[_]`. The same arrow notation already appears in function types, so higher-kinded parameters reuse a spelling readers already know.

## Value-indexed parameters

A bracket parameter can also name a compile-time value when it has a value kind such as `Nat`:

```text
let Vec[T, n : Nat] := data {
  | Nil() -> Vec[T, 0]
  | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]
};
```

Use this when a type should remember shape information such as length or protocol state. Keep normal generics for ordinary reusable code; move to value-indexed parameters only when that extra information helps callers.

## Try it next

- Write one generic function with `[T]`.
- Call it once with `Int`.
- Store it in a record field and call that field with `Int`.
- Write one class that accepts `F : Type -> Type` and instantiate it with `Option`.
- Partially apply a two-argument type constructor and pass it where `Type -> Type` is expected.

## Common mistake

Do not introduce several type parameters before one-parameter generic code feels easy to read.
For higher-kinded parameters, do not write `F[T]` in the parameter list. Write the kind once: `F : Type -> Type`.

## Next

Continue to [Type tests and casts](/docs/language/types/type-tests-and-casts) to handle explicit type-facing checks near boundaries.
