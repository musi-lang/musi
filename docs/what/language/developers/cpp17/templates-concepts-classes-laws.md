---
title: "Templates, Concepts, Classes, and Laws"
description: "Read Templates, Concepts, Classes, and Laws as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 10
slug: "templates-concepts-classes-laws"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

C++ templates and concepts can mix type construction, overload selection, and behavior requirements. Musi splits that apart: data builds values, classes name required behavior, instances supply it.

{{compare:cpp17-templates-concepts-classes-laws}}

## Reading Templates, Concepts, Classes, and Laws from C++17

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
