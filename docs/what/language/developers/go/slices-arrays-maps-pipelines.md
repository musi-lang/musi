---
title: "Slices, Arrays, Maps, and Pipelines"
description: "Read Slices, Arrays, Maps, and Pipelines as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 6
slug: "slices-arrays-maps-pipelines"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this lists, slices, maps, and pipelines example make visible?

{{compare:go-slices-arrays-maps-pipelines}}

## Reading Slices, Arrays, Maps, and Pipelines from Go

On the Musi side, Musi collection examples keep indexing, field access, and pipelines explicit so a reader can see where each value comes from. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not import lazy iterators, stream chains, table conventions, or pointer arithmetic unless the Musi example needs that behavior. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use this shape for prices, stops, animals, files, and other small batches where each step has a name. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Arrays and slices](/learn/book/data/arrays-and-slices)
- [Records](/learn/book/data/records)
- [Dot calls](/learn/book/core/dot-calls)
