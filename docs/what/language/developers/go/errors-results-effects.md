---
title: "Errors, Results, and Effects"
description: "Read Errors, Results, and Effects as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 8
slug: "errors-results-effects"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

Go readers are used to checking nil and a second return value. Musi turns the ordinary missing branch into Option so the caller cannot forget the check after a lookup.

{{compare:go-errors-results-effects}}

## Reading Errors, Results, and Effects from Go

On the Musi side, Musi uses `option.someOf` and `option.noneOf` when absence is expected, so callers handle the empty branch deliberately. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate null, nil, None, or undefined as a quiet ordinary value. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use Option when a badge, ticket, receipt, search result, or lookup may honestly be missing. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
