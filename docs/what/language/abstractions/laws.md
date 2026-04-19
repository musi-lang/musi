---
title: "Laws"
description: "Add semantic expectations after class and instance basics."
group: "Abstractions"
section: "Abstractions"
order: 23
slug: "laws"
summary: "Use laws to document the meaning of an abstraction, not just its shape."
---
Laws state expectations that instances should obey. They are like the rules of a board game: each player may have a different strategy, but the moves still have to follow the same rules.

{{snippet:chapter-laws}}

Laws help readers trust generic code. If equality says two receipts are the same, a law can require that each receipt equals itself. If ordering compares values, laws can rule out impossible cycles.

## What laws buy you

Generic functions often rely on behavior they cannot see. A sorting function trusts that comparison is consistent. A set trusts that equality is stable. Laws write those expectations down.

## Write laws as plain rules

A good law is small and checkable. It should describe one property of the class, not retell the whole class design.

Abstraction chapters explain shared behavior. A record says what fields a value has. A class says what a type can do. An instance gives that behavior for one type. A law names a promise callers depend on.

The false friend is the word class. In Musi it is not an object blueprint. Use records and data for shape. Use classes, instances, and laws when many shapes need to share behavior.
