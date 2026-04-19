---
title: "Unions, Variants, and Pattern Matching"
description: "Read Unions, Variants, and Pattern Matching as a JavaScript and TypeScript habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 7
slug: "unions-variants"
summary: "Translate the JavaScript and TypeScript habit, then use the Musi Book for the full rule."
---

A TypeScript reader brings habits from objects, structural types, unions, undefined, promises, imports, npm packages, and prototype-shaped history. That helps with flexible shapes and fast feedback from type tooling, but the Musi page asks a narrower question: what contract should this named cases and pattern matching example make visible?

{{compare:javascript-typescript-unions-variants}}

## Reading Unions, Variants, and Pattern Matching from JavaScript/TypeScript

On the Musi side, Musi data variants name the cases directly, and `match` reads the case split at the point where the answer is chosen. Read the shared example through JavaScript/TypeScript eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not keep integer tags, string unions, subclass checks, or table marker fields when a data variant is the real shape. For a TypeScript reader, the trap is assuming runtime object shape and compile-time behavior contract are the same thing; Musi `class` is not a JS constructor or TS class; it is a behavior contract implemented separately from record/data shape.

## When this pays off

Use data variants when an order, animal, traffic light, payment state, or parser result has a closed set of cases. The JavaScript/TypeScript instinct still helps here: Keep the TypeScript habit of reading the shape before reading implementation details.

## Keep close

- [Data definitions](/learn/book/data/data-definitions)
- [Patterns](/learn/book/data/patterns)
- [Records](/learn/book/data/records)
