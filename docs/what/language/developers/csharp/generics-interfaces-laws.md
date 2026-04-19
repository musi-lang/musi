---
title: "Generics, Interfaces, and Laws"
description: "Read Generics, Interfaces, and Laws as a C# habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C# Developers"
order: 10
slug: "generics-interfaces-laws"
summary: "Translate the C# habit, then use the Musi Book for the full rule."
---

C# interfaces, extension methods, and generics can all describe shared operations. Musi uses one lane for that idea: a class names the operation and instances provide it for concrete types.

{{compare:csharp-generics-interfaces-laws}}

## Reading Generics, Interfaces, and Laws from C#

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through C# eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a C# reader, the trap is mapping Musi classes to object classes or service containers; Musi `class` is a typeclass-style behavior contract, not a CLR class with fields, constructors, and inheritance.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The C# instinct still helps here: Keep the C# habit of making api shape readable at the call site.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
