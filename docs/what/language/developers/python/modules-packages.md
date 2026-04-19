---
title: "Modules and Packages"
description: "Read Modules and Packages as a Python habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "Python Developers"
order: 11
slug: "modules-packages"
summary: "Translate the Python habit, then use the Musi Book for the full rule."
---

A Python reader brings habits from names, dictionaries, classes, protocols, exceptions, None, imports, and duck typing. That helps with code around domain words before machinery, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:python-modules-packages}}

## Reading Modules and Packages from Python

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through Python eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a Python reader, the trap is trusting runtime convention where Musi expects a visible type or effect; Musi `class` is closer to an explicit protocol/typeclass than a Python class with attributes and methods.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The Python instinct still helps here: Keep the Python habit of naming the real-world thing first.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
