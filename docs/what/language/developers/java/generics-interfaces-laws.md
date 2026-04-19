---
title: "Generics, Interfaces, and Laws"
description: "Read Generics, Interfaces, and Laws as a Java habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Java Developers"
order: 10
slug: "generics-interfaces-laws"
summary: "Translate the Java habit, then use the Musi Book for the full rule."
---

Java interfaces describe behavior through nominal types and implementations. Musi classes serve the behavior-contract role, while records/data handle the value shapes Java classes often contain.

{{compare:java-generics-interfaces-laws}}

## Reading Generics, Interfaces, and Laws from Java

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through Java eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a Java reader, the trap is reading Musi `class` as a nominal object type with constructors and fields; Musi `class` is a behavior contract supplied by instances; records and data model object shape.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The Java instinct still helps here: Keep the Java habit of naming APIs for future readers.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
