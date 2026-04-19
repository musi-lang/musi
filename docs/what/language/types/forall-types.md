---
title: "Forall Types"
description: "Read explicit universal type forms without turning them into everyday ceremony."
group: "Types"
section: "Types"
order: 22
slug: "forall-types"
summary: "Use `forall` when a type expression must bind a type variable explicitly."
---
`forall` types describe code that works for every type matching the shape. They are the type-level version of saying, “I do not care what is in the box; I only promise to handle the box consistently.”

{{snippet:chapter-forall-types}}

This is useful for reusable helpers whose behavior does not depend on the specific value type. The more a function truly ignores the details, the stronger the promise becomes.

## Universal promises

An identity function can return whatever it receives. A wrapper can store any value and give it back. These promises are broad, but they are also strict: the code cannot secretly depend on a specific type.

## Read the quantifier as a boundary

When you see `forall`, ask what information the function has refused to know. That refusal is what makes the function reusable.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
