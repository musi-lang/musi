---
title: "Headers, Modules, and Packages"
description: "Read Headers, Modules, and Packages as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 12
slug: "headers-modules-packages"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

A C++ reader brings habits from RAII, overload sets, templates, references, optional values, exceptions, and object hierarchies. That helps with ownership stories, value categories, and compile-time abstractions, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:cpp17-headers-modules-packages}}

## Reading Headers, Modules, and Packages from C++17

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
