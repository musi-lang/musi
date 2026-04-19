---
title: "Blocks, Branching, and Loops"
description: "Read Blocks, Branching, and Loops as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 3
slug: "blocks-branching-loops"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this branching and block results example make visible?

{{compare:go-blocks-branching-loops}}

## Reading Blocks, Branching, and Loops from Go

On the Musi side, Musi blocks and matches are expressions when they produce a value; the branch answer matters more than the statement container. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not carry over temporary variables whose only job was to smuggle a branch result out of a statement block. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use this shape when a route fee, access decision, or small rule table chooses one value from several cases. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
- [Patterns](/learn/book/data/patterns)
- [Operators](/learn/book/core/operators)
