---
title: "Modules, Packages, and Visibility"
description: "Read Modules, Packages, and Visibility as a Rust habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Rust Developers"
order: 9
slug: "modules-packages"
summary: "Translate the Rust habit, then use the Musi Book for the full rule."
---

A Rust reader brings habits from ownership, enums, traits, Result, modules, pattern matching, and explicit unsafe blocks. That helps with the type that carries the invariant, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:rust-modules-packages}}

## Reading Modules, Packages, and Visibility from Rust

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through Rust eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a Rust reader, the trap is expecting Musi syntax to mirror Rust even when the ideas are separated differently; Musi `class` is closest to a Rust trait plus law text; instances play the role of implementations for behavior.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The Rust instinct still helps here: Keep the Rust habit of asking which type carries the invariant.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
