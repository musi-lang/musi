---
title: "Effects"
description: "Introduce effect vocabulary before using clauses or handlers."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 24
slug: "effects"
summary: "Understand effects as requests for work, not immediate hidden side effects."
---
Effects describe work that reaches outside plain value calculation. Reading the clock, asking the environment for a variable, printing a line, or requesting randomness all depend on something beyond the current expression.

{{snippet:chapter-effects}}

Think of an effect as a service desk. A function can fill out a request, but some handler or runtime must actually answer it. That separation lets ordinary code stay clear about what it wants without pretending that outside work is the same as arithmetic.

## Why requests are named

Named operations make boundaries visible. `Clock.tick` tells the reader that time is involved. `Runtime.envGet` tells the reader that the process environment is involved. That is much clearer than hiding those facts behind a helper that looks pure.

## Keeping business rules separate

A shipping estimate may need the current date, but the rule for choosing the delivery window should not be tangled with the mechanism that reads the clock. Ask for the outside value at the edge, then pass ordinary values into ordinary functions.

## Reading declarations inside an effect

Each `let` inside an effect body describes one operation that may be requested. A large runtime effect can list many operations, just like a city office lists many counters: permits, tax forms, records, and payments. The list is not the implementation; it is the menu of requests.
