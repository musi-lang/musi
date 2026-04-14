---
title: "Using"
description: "Learn using syntax before full handlers so capability requirements stay visible."
group: "Effects and runtime"
section: "Effects and runtime"
order: 25
slug: "using"
summary: "Read and write using clauses as explicit capability flow."
---

{{snippet:chapter-using}}

## What

`using` clauses make capability requirements visible in the type-level surface of a definition.
A function that requests effectful work can say so directly instead of leaving the dependency ambient or hidden.
That makes effect requirements part of the signature a reader sees first.

## Why

Once users understand effect requests, the next question is "how do I know this function needs that capability?"
A signature-level answer scales better than relying on comments or hidden convention.
This page should show that effectful code advertises its needs instead of surprising the caller later.

## How

Read `using { Console }` as requirement attached to the function, not as runtime argument list.
Even if the example is small, the lesson is practical: signatures can tell you what must be available before the body can request certain operations.
When designing APIs, add `using` where capability dependence is real and useful for callers to know up front.

## Try it

- Write one function signature with `using` clause.
- Add one requested effect inside body.
- Compare signature with equivalent hidden-dependency story.

## Common mistake

Do not treat `using` as optional decoration when function genuinely depends on capability availability.

## Next

Continue to [Handlers](/docs/language/effects-runtime/handlers) to resolve those requests at a boundary that can choose policy.
