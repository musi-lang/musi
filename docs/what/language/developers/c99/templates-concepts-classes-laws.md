---
title: "Macros, Generics, Classes, and Laws"
description: "Read Macros, Generics, Classes, and Laws as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 10
slug: "templates-concepts-classes-laws"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C reader brings habits from headers, translation units, pointers, arrays, `errno`, sentinel returns, and small functions that trust callers. That helps with cost, layout, and the exact boundary where bytes cross into code, but the Musi page asks a narrower question: what contract should this generic behavior contracts example make visible?

{{compare:c99-templates-concepts-classes-laws}}

## Reading Macros, Generics, Classes, and Laws from C99

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
