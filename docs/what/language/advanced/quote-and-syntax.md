---
title: "Quote and Syntax"
description: "Introduce quote and syntax work late so beginners are not overloaded too early."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "quote-and-syntax"
summary: "Treat code as data only after ordinary code reading feels natural."
---

{{snippet:chapter-quote-and-syntax}}

## Boundary Tool

`quote` turns code shape into syntax data you can inspect, build, or reuse.
The first snippet shows simplest quoted expression, and the second shows interpolation with `#(...)` inside quoted form.
This chapter belongs late because it asks you to reason about code as data rather than just running code.

## When to Reach for It

Metaprogramming questions show up after ordinary code already feels familiar.
At that point users need examples that explain both power and boundary: quoting is useful, but it is not default way to write everyday logic.
A focused page keeps this tool available without overwhelming readers who are still stabilizing basic syntax.

## Read the Boundary

Read `quote (x + 1);` as syntax value representing expression shape.
Then read `#(delta)` or `#(x)` inside quoted form as splice points where surrounding values contribute pieces to generated syntax.
Use `comptime quote { ... }` when quoted module items should expand before normal checking continues.
When experimenting, start with very small quoted expressions and ask what syntax object each quote should represent before building larger templates.

## Small Exercise

- Quote one simple expression.
- Add one splice inside a quoted template.
- Compare quoted template with handwritten equivalent shape.

## Mistake to Avoid

Do not reach for quote when an ordinary function or data value already solves the problem more directly.

## Next Page

Continue to [Comptime](/learn/book/advanced/comptime) to run code during compilation and expand generated syntax.
