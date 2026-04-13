---
title: "Effects and handlers"
description: "Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code."
group: "Effects"
section: "Effects"
order: 12
slug: "effects-and-handlers"
summary: "The main Musi differentiator, shown with real syntax."
---

Effects are part of ordinary Musi code. Define an effect, `perform` an operation, then `handle` it at a boundary.

## Handle first

{{snippet:handle-console}}

Handlers decide what to do with an operation request. `resume` continues execution with a value.

## Define effect

{{snippet:effect-console}}

## Perform operation

{{snippet:perform-console}}

## Compare

{{example:effect-handle}}

Effects are useful when direct calls would otherwise drag boundary logic through many layers of code.

## Try it

{{try:effects-and-handlers}}

## Next step

Read all three snippets end-to-end, then continue to [Attributes and foreign declarations](/docs/attributes-and-foreign).
