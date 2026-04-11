---
title: "Effects and handlers"
description: "Use <code>effect</code>, <code>perform</code>, <code>handle</code>, and <code>resume</code> as part of normal Musi code."
group: "Effects"
section: "Effects"
order: 9
slug: "effects-and-handlers"
summary: "The main Musi differentiator, shown with real syntax."
---

## What
Effects are built into the language as part of normal flow control:
you define an effect, perform it, and handle it at a boundary.

## Why
This model separates “what happened” from “how to handle it,” which keeps business logic clearer as projects grow.

{{snippet:effect-console}}

## How
Define one effect family, perform operations, and add handlers for policy (logging, fallback, default values, reporting).

{{snippet:perform-console}}

## When
Use effects for cross-cutting concerns:
resource usage, command routing, telemetry, and deferred behavior.

{{snippet:handle-console}}

## Analogy
Comparable to middleware stacks in web frameworks, but in expression-level form.

## Try it
Read all three snippets end-to-end, then continue to [Types and generics](/docs/types).

Continue to [Types and generics](/docs/types).
