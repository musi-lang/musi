---
title: "Records, Structs, and Field Updates"
description: "Read Records, Structs, and Field Updates as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 4
slug: "records-structs"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

A Rust reader brings habits from ownership, enums, traits, Result, modules, pattern matching, and explicit unsafe blocks. That helps with the type that carries the invariant, but the Musi page asks a narrower question: what contract should this field-shaped data example make visible?

{{compare:rust-records-structs}}

## Reading Records, Structs, and Field Updates from Rust

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Records](/learn/book/data/records)
- [Classes](/learn/book/abstractions/classes)
- [Patterns](/learn/book/data/patterns)
