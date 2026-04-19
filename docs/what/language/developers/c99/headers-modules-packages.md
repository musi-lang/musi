---
title: "Headers, Modules, and Packages"
description: "Read Headers, Modules, and Packages as a C99 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C Developers"
order: 12
slug: "headers-modules-packages"
summary: "Translate the C99 habit, then use the Musi Book for the full rule."
---

A C header announces names but does not own runtime meaning by itself. Musi imports name the module value directly, so the use site shows which package owns `option`, `runtime`, or `text`.

{{compare:c99-headers-modules-packages}}

## Reading Headers, Modules, and Packages from C99

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through C99 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a C reader, the trap is treating convention as a contract; Musi `class` is not a C struct with function pointers; records/data carry shape, classes name behavior a type can provide.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The C99 instinct still helps here: Keep the C habit of asking where memory, symbols, and failures come from.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
