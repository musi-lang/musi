---
title: "Dependent types"
description: "Use values in type positions when shape, size, or protocol state should be visible in the type."
group: "Types"
section: "Types"
order: 23
slug: "dependent-types"
summary: "Use value-indexed types, indexed data results, `partial`, and `~=` without turning Musi into a proof assistant."
---

{{snippet:chapter-dependent-types}}

## In this chapter

Musi allows type parameters to be ordinary types or value-shaped indices. A type parameter such as `T` has kind `Type`. Universe names such as `Type0` are accepted when you want to make the level explicit. A value index such as `n : Nat` has kind `Nat`, so it can describe a length, state number, port number, protocol phase, or other compile-time value.

This is dependent typing in the practical language sense: useful values can appear in types. It is not a proof-assistant surface. You write programs, data shapes, and constraints; you do not write proof terms just to make normal code compile.

## Value parameters in type parameter lists

Use `name : Kind` in brackets when the parameter is not just another concrete type:

```text
let Vec[T, n : Nat] := data {
  | Nil() -> Vec[T, 0]
  | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]
};
```

Read that as:

- `T` is an item type.
- `n : Nat` is a compile-time natural number.
- `Nil` constructs a `Vec[T, 0]`.
- `Cons` takes a tail of length `n` and returns length `n + 1`.

The constructor result after `->` is part of the variant declaration. It lets a variant say which indexed instance of the data type it creates.

## Type equality constraints

Use `~=` for solver-facing type equality. It reads as "has the same type-level shape as" and belongs in constraints and type reasoning, not in ordinary value comparison. Runtime equality stays `=`.

```text
let choose[A, B] (left : A, right : B) : A
where A ~= B
:= left;
```

Use this when two separately named type expressions must be known equal by the type checker.

## `partial` marks runtime-only definitions

A definition referenced from a type must be usable by the type checker. Put `partial` on a `let` when the definition is intentionally runtime-only, may diverge, or depends on behavior that should not be evaluated during type checking.

```text
partial let parsePort(text : String) : Int := 0;
```

`partial` is a modifier like `foreign`, but the two do not combine. A foreign binding is already runtime-bound through an ABI boundary.

## What Musi does not ask you to write

Musi does not make everyday users write `refl` proofs, tactic scripts, or theorem-prover terms. If two indexed types are equal by the language rules and available total definitions, the solver should see that. If code is runtime-only, mark it `partial` instead of pretending it can run in the type checker.

## Try it next

- Write an indexed data type with one `Nat` parameter.
- Give each variant an explicit `->` result when it changes the index.
- Use `partial` on parsing, FFI wrappers, or non-total helpers that should stay out of type computation.
- Use `~=` only when you need type-level equality, not normal value equality.

## Common mistake

Do not use `:` for constraints. `:` annotates a name with a type or kind. Constraints belong after `where`, and type equality uses `~=`.

## Next

Continue to [Classes](/docs/language/abstractions/classes) to see how reusable behavior contracts sit on top of types.
