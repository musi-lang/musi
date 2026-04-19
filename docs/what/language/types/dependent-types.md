---
title: "Dependent Types"
description: "Use values in type positions when shape, size, or protocol state should be visible in the type."
group: "Types"
section: "Types"
order: 23
slug: "dependent-types"
summary: "Use value-indexed types, indexed data results, `partial`, and `~=` without turning Musi into a proof assistant."
---
Dependent types let types mention values when the shape of the program needs that precision. They are useful for sizes, labels, indexes, or other facts that must travel with a type.

{{snippet:chapter-dependent-types}}

Use this power when the value-level fact prevents real mistakes. A fixed-size batch, a measured unit, or a known field name can be clearer when the type carries the fact.

## Precision has a cost

Do not move every ordinary value into the type system. A person's age changes, a cart total changes, and a search query is usually just data. Dependent information should describe structure, not everyday noise.

## Good candidates

Array lengths, protocol states, route labels, and schema keys are common candidates. They are small facts with large consequences when wrong.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
