---
title: "Functions"
description: "Define reusable functions, learn named arguments, and understand why parameter names matter."
group: "Core Syntax"
section: "Core Syntax"
order: 9
slug: "functions"
summary: "Functions are ordinary `let` bindings with parameters, result types, and expression bodies."
---
Functions turn a question into a reusable action. Instead of writing the same price calculation in three checkout screens, name it once and call it wherever a cart needs a total.

{{snippet:chapter-functions}}

Parameters describe what the function needs from the outside world. The return type describes what it promises to give back. That promise matters more than the function body when another file calls it.

## Choosing a function boundary

Make a function when a few lines have a name in ordinary speech. `totalInvoice`, `isWeekend`, and `formatAnimalTag` are good signals. If the best name is vague, the function probably contains more than one idea.

## Small promises compose

A large process becomes readable when each function keeps a small promise. A booking flow can check the date, choose a room, compute the price, and build a confirmation. Each function can be tested and replaced without rewriting the whole flow.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
