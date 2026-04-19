---
title: "Generics"
description: "Introduce type parameters after annotations and inference make sense."
group: "Types"
section: "Types"
order: 20
slug: "generics"
summary: "Write reusable functions over many types without losing clarity."
---
Generics let one definition work with many types. A box can hold a book, a toy, or a receipt; the rules for storing and returning the item may not care which one it is.

{{snippet:chapter-generics}}

Use generics when the structure is the same and only the contained type changes. Do not make code generic just because it feels flexible; generic code should still have a clear job.

## Real reusable shapes

Lists, options, results, boxes, and identity helpers are natural generic shapes. They talk about how values move, not about one particular domain.

## Type parameters need meaning

Short names such as `T` are fine when the role is obvious. When there are several type parameters, choose names that explain the relationship: `Key`, `Value`, `Input`, `Output`, or a domain-specific name.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
