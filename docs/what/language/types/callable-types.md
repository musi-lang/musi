---
title: "Callable Types"
description: "Read pure and effectful function types as part of ordinary type syntax."
group: "Types"
section: "Types"
order: 18
slug: "callable-types"
summary: "Use `T -> U` for pure callables and `T ~> U` for effectful callables."
---
Callable types describe values that can be called like functions. They matter when behavior is passed around: sorting people, filtering animals, formatting payments, or choosing a retry rule.

{{snippet:chapter-callable-types}}

A callable type is a contract for behavior. It says what inputs the behavior accepts and what result it returns, even when the concrete function is chosen later.

## Behavior as a value

A shop may pass a discount rule into checkout. A shelter may pass an adoption filter into a search page. The calling code does not need to know which rule was chosen, only that it has the right callable shape.

## Keep signatures readable

If a callable type becomes long, name it. A named type such as `PriceRule` or `AnimalFilter` can make a declaration readable again and give the behavior a place in the domain language.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
