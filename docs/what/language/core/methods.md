---
title: "Methods"
description: "Learn Musi's attached-method model after plain functions and calls."
group: "Core syntax"
section: "Core syntax"
order: 11
slug: "methods"
summary: "Use receiver-prefixed methods and dot calls without needing an impl block."
---

{{snippet:chapter-methods}}

## In this chapter

Methods in Musi are receiver-prefixed function definitions.
Instead of inventing a separate `impl` block or class body, the receiver appears right in the definition, and the call site uses dot syntax on the value.
That keeps method behavior close to ordinary function behavior while making receiver visible.

## Why it matters

Users coming from object-heavy languages expect one model; users coming from functional languages expect another.
This chapter should show that Musi's method surface is simpler than both expectations: define behavior with a receiver parameter, then call it from the value you already have.
That answers "how do I write `x.abs()`?" without dragging in more abstraction machinery.

## Walk through it

Read `let (self : Int).abs () : Int := self;` as a function whose first visible role is receiver.
A receiver can be marked `mut` when method body needs mutable receiver behavior.
Then read `one.abs();` as method call on value `one`, not as magical property lookup.
When deciding between plain function and method, prefer method when receiver-led reading is clearer at call site.

## Try it next

- Define one receiver method for a simple type.
- Call it from a named value.
- Compare that call with equivalent plain function style.

## Common mistake

Do not assume methods require a separate container type declaration before they can exist.

## Next

Continue to [Records](/docs/language/data/records) to move from scalar values into labeled data.
