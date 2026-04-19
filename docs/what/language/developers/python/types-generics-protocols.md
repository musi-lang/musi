---
title: "Types, Generics, and Protocols"
description: "Read Types, Generics, and Protocols as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 10
slug: "types-generics-protocols"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

A Python reader brings habits from names, dictionaries, classes, protocols, exceptions, None, imports, and duck typing. That helps with code around domain words before machinery, but the Musi page asks a narrower question: what contract should this generic behavior contracts example make visible?

{{compare:python-types-generics-protocols}}

## Reading Types, Generics, and Protocols from Python

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
