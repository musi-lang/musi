---
title: "Packages, Imports, and Exports"
description: "Read Packages, Imports, and Exports as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 13
slug: "packages-imports-exports"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

A Go reader brings habits from packages, structs, slices, nil, multiple returns, interfaces, goroutines, channels, and explicit errors. That helps with small named operations and direct data flow, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:go-packages-imports-exports}}

## Reading Packages, Imports, and Exports from Go

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
