---
title: "Quote and syntax"
description: "Introduce quote and syntax work late so beginners are not overloaded too early."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 32
slug: "quote-and-syntax"
summary: "Treat code as data only after ordinary code reading feels natural."
---

{{snippet:chapter-quote-and-syntax}}

## In this chapter

`quote` turns code shape into syntax data you can inspect, build, or reuse.
The first snippet shows simplest quoted expression, and the second shows interpolation with `#(...)` inside quoted form.
This chapter belongs late because it asks you to reason about code as data rather than just running code.

## Why it matters

Metaprogramming questions show up after ordinary code already feels familiar.
At that point users need examples that explain both power and boundary: quoting is useful, but it is not default way to write everyday logic.
A focused page keeps this tool available without overwhelming readers who are still stabilizing basic syntax.

## Walk through it

Read `quote (x + 1);` as syntax value representing expression shape.
Then read `#(delta)` or `#(x)` inside quoted form as splice points where surrounding values contribute pieces to generated syntax.
When experimenting, start with very small quoted expressions and ask what syntax object each quote should represent before building larger templates.

## Try it next

- Quote one simple expression.
- Add one splice inside a quoted template.
- Compare quoted template with handwritten equivalent shape.

## Common mistake

Do not reach for quote when an ordinary function or data value already solves the problem more directly.

## Next

Continue to [Templates and splices](/docs/language/advanced/templates-and-splices) to separate text interpolation from syntax splicing.
