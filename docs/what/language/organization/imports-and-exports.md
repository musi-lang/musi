---
title: "Imports and Exports"
description: "Use imports and exports after package shape is clear."
group: "Code Organization"
section: "Code Organization"
order: 17
slug: "imports-and-exports"
summary: "Bring code in explicitly and expose only what other files need."
---

Imports bring another module exported names into reach. Exports decide which names leave the current module.

{{snippet:chapter-imports-and-exports}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Type Annotations](/learn/book/types/foundations/type-annotations).
