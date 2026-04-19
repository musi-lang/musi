---
title: "Variables, Short Declarations, and Mutation"
description: "Read Variables, Short Declarations, and Mutation as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 4
slug: "variables-short-declarations-mutation"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this fresh values and mutation example make visible?

{{compare:go-variables-short-declarations-mutation}}

## Reading Variables, Short Declarations, and Mutation from Go

On the Musi side, Musi makes mutation explicit with `mut` and assignment; ordinary `let` names read as stable facts. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate every rebinding habit into mutation. A new receipt, label, or counter snapshot can be a fresh name. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use mutation when the domain really changes over time, such as queue depth or a buffer cursor. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Mutation](/learn/book/start/mutation)
- [Values and let](/learn/book/start/values-and-let)
- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
