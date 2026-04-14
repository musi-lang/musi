---
title: "Calls"
description: "Learn call syntax as its own reading skill before adding dot calls."
group: "Core syntax"
section: "Core syntax"
order: 10
slug: "calls"
summary: "Call functions directly and follow argument flow left to right."
---

{{snippet:chapter-calls}}

## What

A call applies a function to arguments.
This page reuses a tiny function example because the point is not more syntax surface; the point is learning to read `twice(21)` as value flow from argument into function and back out as result.
Calls show up everywhere, so this reading habit must become boring fast.

## Why

Many beginner questions are really call-reading questions: "where does this value go?", "what is input here?", or "why are parentheses here but not there?"
If call syntax is only mentioned in passing, those questions keep interrupting later chapters.
A short focused chapter pays off because functions, constructors, stdlib helpers, and methods all build on same habit of tracking inputs and outputs.

## How

Look at definition first, then read call left to right.
`twice` names the function, `(21)` supplies one argument, and the whole expression evaluates to returned result.
When calls get larger, keep naming intermediate values so you are still reading one input step at a time rather than decoding a pile of nested punctuation.

## Try it

- Write one small function.
- Call it with one literal argument.
- Bind call result to a name before doing anything larger.

## Common mistake

Do not confuse function definition syntax with function call syntax just because both sit near same name.

## Next

Continue to [Methods](/docs/language/core/methods) to see how Musi attaches behavior to a receiver and calls it with dot syntax.
