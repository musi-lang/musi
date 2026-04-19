---
title: "Goroutines, Channels, Context, and Effects"
description: "Read Goroutines, Channels, Context, and Effects as a Go habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Go Developers"
order: 9
slug: "goroutines-channels-context-effects"
summary: "Translate the Go habit, then use the Musi Book for the full rule."
---

Go makes concurrency explicit with goroutines, channels, and contexts. Musi effect pages are about a different explicitness: naming the request for runtime work before deciding how it is handled.

{{compare:go-goroutines-channels-context-effects}}

## Reading Goroutines, Channels, Context, and Effects from Go

On the Musi side, Musi names outside work with effects and `request`, instead of hiding time, files, console work, or services inside an ordinary-looking call. Read the shared example through Go eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not make an effect look pure. A clock, process, network service, or console changes the trust boundary. For a Go reader, the trap is using absence or failure as a side channel because Go makes that cheap; Musi `class` is closer to an explicit interface constraint with instances; it is not a struct and not a method set attached by package convention.

## When this pays off

Use effects when the program asks the runtime, OS, user, or another service for an answer. The Go instinct still helps here: Keep the Go habit of writing the small thing first and naming package boundaries clearly.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
