---
title: "Generics"
description: "Introduce type parameters after annotations and inference make sense."
group: "Types"
section: "Types"
order: 20
slug: "generics"
summary: "Write reusable functions over many types without losing clarity."
---

Generics let one definition work for many types. Start with the direct case: `identityFn[T]` names a type parameter, and `identityFn[Int](8080)` chooses `Int` for that call.

{{snippet:chapter-generics}}

`tools` shows that a generic function can be stored in an ordinary record and called through a field. The type argument still belongs at the call site, so `tools.identity[Int](port)` reads the same way as the direct call.

`Box1` and `Keeps` show the larger shape. `Box1` is not a single finished type; it is a type constructor that becomes `Box1[Int]` when given `Int`. That is why `Keeps` accepts `F : Type -> Type`: the class needs something that can build a concrete type from another type.

Use generics when the operation is truly the same across several types. Keep the first examples concrete, then add type-constructor parameters only when the abstraction needs to talk about containers, wrappers, or other type families.

Continue to [Callable Types](/learn/book/types/foundations/callable-types).
