---
title: "Promises, Async, and I/O Boundaries"
description: "Read Promises, Async, and I/O Boundaries as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 9
slug: "promises-effects"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

Promises make time and outside work composable in TypeScript. Musi uses effects to name the outside request first, then lets handlers or runtime code decide how it runs.

{{compare:javascript-typescript-promises-effects}}

## Reading Promises, Async, and I/O Boundaries from JavaScript/TypeScript

On the Musi side, Musi names outside work with effects and `request`, instead of hiding time, files, console work, or services inside an ordinary-looking call. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not make an effect look pure. A clock, process, network service, or console changes the trust boundary. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use effects when the program asks the runtime, OS, user, or another service for an answer. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Effects](/learn/book/effects-runtime/effects)
- [Using](/learn/book/effects-runtime/using)
- [Handlers](/learn/book/effects-runtime/handlers)
