---
title: "Objects, Records, and Field Updates"
description: "Read Objects, Records, and Field Updates as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 4
slug: "objects-records"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

TypeScript structural objects make shape feel lightweight. Musi keeps that benefit, but record shape does not also imply methods, prototype behavior, or package identity.

{{compare:javascript-typescript-objects-records}}

## Reading Objects, Records, and Field Updates from JavaScript/TypeScript

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Records](/learn/book/data/records)
- [Classes](/learn/book/abstractions/classes)
- [Patterns](/learn/book/data/patterns)
