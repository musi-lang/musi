---
title: "Interfaces, Classes, Instances, and Laws"
description: "Read Interfaces, Classes, Instances, and Laws as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 10
slug: "interfaces-classes-laws"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

Go interfaces are satisfied implicitly by method sets. Musi classes are explicit behavior requirements, and instances are the visible evidence that a type supplies the operation.

{{compare:go-interfaces-classes-laws}}

## Reading Interfaces, Classes, Instances, and Laws from Go

On the Musi side, Musi classes name behavior a type can provide, instances provide it, and laws document promises callers rely on. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not confuse a Musi class with stored object state, inheritance, or a metatable. Shape and behavior are separate. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use classes when many types share an operation such as compare, show, step, encode, or measure. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Classes](/learn/book/abstractions/classes)
- [Instances](/learn/book/abstractions/instances)
- [Laws](/learn/book/abstractions/laws)
