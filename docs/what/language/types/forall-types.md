---
title: "Forall types"
description: "Read explicit universal type forms without turning them into everyday ceremony."
group: "Types"
section: "Types"
order: 22
slug: "forall-types"
summary: "Use `forall` when a type expression must bind a type variable explicitly."
---

{{snippet:chapter-forall-types}}

## In this chapter

`forall` introduces a type variable inside a type expression.
Most day-to-day generic functions use bracket parameters on `let` definitions.
`forall` is the explicit form for type-level expressions that need to name the variable directly.

## Why it matters

Some APIs talk about polymorphic values themselves, not just generic definitions.
When that happens, the type needs a place to say "this works for every `T`."
`forall(T : Type) -> T -> T` makes that binding visible.
Higher-kinded binders use the same form with an arrow kind: `forall(F : Type -> Type) -> F[Int] -> F[Int]`.

## Walk through it

Read `forall(T : Type) -> ...` as "for every type `T`, the rest of the type follows."
Read `forall(F : Type -> Type) -> ...` as "for every unary type constructor `F`, the rest of the type follows."
Then read `T -> T` as the callable shape under that binding.
Use this form when the type is the subject of the code, not just a detail inferred from a generic function.

The bracket form on a definition and the `forall` form in a type describe the same idea from two sides:

```text
let identityFn[T] (input : T) : T := input;
let identityType := forall(T : Type) -> T -> T;
```

`identityFn` is a polymorphic value. If you move it through another value, the `forall` shape moves with it.

## Try it next

- Read one generic function signature.
- Write the matching callable type with `forall`.
- Store the generic function in a record and read the field type as a `forall` value.

## Common mistake

Do not add `forall` to every generic function definition. Bracket parameters already cover the common case.

## Next

Continue to [Classes](/docs/language/abstractions/classes) to move from type parameters into reusable behavior contracts.
