---
title: "Effects and handlers"
description: "Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code."
group: "Effects"
section: "Effects"
order: 12
slug: "effects-and-handlers"
summary: "The main Musi differentiator, shown with real syntax."
---

## What
Effects are built into the language as part of normal flow control:
you define an effect, perform it, and handle it at a boundary.

## When
Use effects for cross-cutting concerns:
resource usage, command routing, telemetry, and deferred behavior.

{{snippet:handle-console}}

## Why
This model separates “what happened” from “how to handle it,” which keeps business logic clearer as projects grow.

{{snippet:effect-console}}

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Define one effect family, perform operations, and add handlers for policy (logging, fallback, default values, reporting).

{{snippet:perform-console}}

{{example:effect-handle}}

## Analogy
From ground level, Earth can look flat; as altitude increases, the curve becomes obvious. Effects are similar: in tiny examples they can look like callback plumbing, but as programs scale the separation between operation requests and handler policy becomes much clearer.

## Try it
Read all three snippets end-to-end, then continue to [Attributes and foreign declarations](/docs/attributes-and-foreign).
