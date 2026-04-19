---
title: "Dot Calls"
description: "Learn Musi dot-callable model after plain functions and calls."
group: "Core Syntax"
section: "Core Syntax"
order: 11
slug: "dot-calls"
summary: "Use dot-callable functions through dot notation without needing an impl block."
---

Dot calls use receiver-pattern methods or explicitly visible receiver-first functions. Receiver-pattern methods attach callable lookup to a receiver type without reserving a `self` keyword.

`point.move(1)` resolves through UDNS and inserts `point` as the first argument. `Point.move(point, 1)` uses the receiver type namespace explicitly. Attached methods do not pollute bare function lookup, so `move(point, 1)` only works when a callable named `move` is explicitly bound or imported.

{{snippet:chapter-methods}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Records](/learn/book/data/modeling/records).
