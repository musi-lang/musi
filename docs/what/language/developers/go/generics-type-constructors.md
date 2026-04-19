---
title: "Generics and Type Constructors"
description: "Read Generics and Type Constructors as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 11
slug: "generics-type-constructors"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this field-shaped data example make visible?

{{compare:go-generics-type-constructors}}

## Reading Generics and Type Constructors from Go

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Generics](/learn/book/types/generics)
- [Classes](/learn/book/abstractions/classes)
- [Laws](/learn/book/abstractions/laws)
