---
title: "Values, Functions, and Final Expressions"
description: "Read Values, Functions, and Final Expressions as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 2
slug: "values-functions"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

A TypeScript reader brings habits from objects, structural types, unions, undefined, promises, imports, npm packages, and prototype-shaped history. That helps with flexible shapes and fast feedback from type tooling, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:javascript-typescript-values-functions}}

## Reading Values, Functions, and Final Expressions from JavaScript/TypeScript

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Values and let](/learn/book/start/values-and-let)
- [Functions](/learn/book/core/functions)
- [Calls](/learn/book/core/calls)
