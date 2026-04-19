---
title: "Modules, Packages, and Imports"
description: "Read Modules, Packages, and Imports as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 10
slug: "modules-packages"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

A TypeScript reader brings habits from objects, structural types, unions, undefined, promises, imports, npm packages, and prototype-shaped history. That helps with flexible shapes and fast feedback from type tooling, but the Musi page asks a narrower question: what contract should this module and package boundaries example make visible?

{{compare:javascript-typescript-modules-packages}}

## Reading Modules, Packages, and Imports from JavaScript/TypeScript

On the Musi side, Musi imports bring named module values into scope, so package boundaries stay visible where code uses them. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not rebuild a source-language global namespace, header include pile, or barrel file when one import names the owner. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use modules when billing, routing, text, option, runtime, or encoding code has an owner worth naming. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Imports and exports](/learn/book/organization/imports-and-exports)
- [Packages](/learn/book/organization/packages)
- [Foreign](/learn/book/advanced/foreign)
