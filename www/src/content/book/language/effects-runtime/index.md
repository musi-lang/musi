---
title: "Effects and runtime"
description: "Understand effects, using, handlers, foundation, runtime, and stdlib layering."
group: "Effects and runtime"
section: "Effects and runtime"
order: 7
slug: "effects-runtime"
summary: "Make effect flow explicit, then place runtime and stdlib on top of that model."
---

This part explains explicit capability flow and the layers built around it.
Effects describe requested work, `using` surfaces required capabilities, handlers resolve requests, and foundation/runtime/stdlib pages place imports in their proper layer.
The goal is to make boundary thinking readable instead of mystical.

## What

This section teaches effect requests, capability requirements, request handling, and module layering from core to runtime to stdlib.
It is one of richest parts of the book, but every page should still answer one practical question.

## Why

Effects and runtime topics become intimidating when all boundaries are introduced at once.
Users then ask whether something is built in, imported, handled, runtime-backed, or just standard library code.
This section prevents that pile-up by separating each concern while keeping one coherent model of explicit capability flow.

## How

Follow the order.
Learn effect requests before handlers, understand `using` before resolving capabilities, and keep module layers distinct when reading imports.
Whenever a page feels abstract, come back to concrete question: what work is being requested, and who is responsible for providing it?
