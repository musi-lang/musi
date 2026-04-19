---
title: "`let`, `const`, and Mutable State"
description: "Read `let`, `const`, and Mutable State as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 3
slug: "state"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

A TypeScript reader brings habits from objects, structural types, unions, undefined, promises, imports, npm packages, and prototype-shaped history. That helps with flexible shapes and fast feedback from type tooling, but the Musi page asks a narrower question: what contract should this fresh values and mutation example make visible?

{{compare:javascript-typescript-state}}

## Reading `let`, `const`, and Mutable State from JavaScript/TypeScript

On the Musi side, Musi makes mutation explicit with `mut` and assignment; ordinary `let` names read as stable facts. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not translate every rebinding habit into mutation. A new receipt, label, or counter snapshot can be a fresh name. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use mutation when the domain really changes over time, such as queue depth or a buffer cursor. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Mutation](/learn/book/start/mutation)
- [Values and let](/learn/book/start/values-and-let)
- [Blocks and expressions](/learn/book/start/blocks-and-expressions)
