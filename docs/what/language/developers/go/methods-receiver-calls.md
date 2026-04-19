---
title: "Methods and Receiver Calls"
description: "Read Methods and Receiver Calls as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 12
slug: "methods-receiver-calls"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:go-methods-receiver-calls}}

## Reading Methods and Receiver Calls from Go

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Calls](/learn/book/core/calls)
- [Dot calls](/learn/book/core/dot-calls)
- [Functions](/learn/book/core/functions)
