---
title: "Functions"
description: "Define reusable functions, learn named arguments, and understand why parameter names matter."
group: "Core syntax"
section: "Core syntax"
order: 9
slug: "functions"
summary: "Functions are ordinary `let` bindings with parameters, result types, and expression bodies."
---

{{snippet:chapter-functions}}

A Musi function is still a `let` binding.
The difference is that the bound value accepts parameters and produces a result.
That keeps the language surface small while still letting you write real reusable code.

## In this chapter

Read a function definition in pieces:

- function name
- parameter list
- optional result type annotation
- body expression

Musi supports named arguments at ordinary call sites.
That means parameter names are not just internal decoration.
They are part of the public call surface when you choose to call with labels.

For example, if a function is defined with parameters like `port` and `secure`, callers can write the positional form or the labeled form:

- `render(8080, .True)`
- `render(port := 8080, secure := .True)`

## Why it matters

Named arguments help most when a call has several parameters of similar shape.
They make call sites read like intent instead of like memory work.

They also make one thing important: renaming a public parameter name changes call sites that use named arguments.

## Walk through it

Start with the plain definition.
Then read the call site.
If the call uses names, match each label back to the parameter list.

Musi keeps one simple mixing rule:

- positional arguments first
- named arguments after

That means code like `f(1, mode := "fast")` is fine, but once named arguments begin, later positional arguments are not.

Pipelines still work with this model.
A piped value becomes the first positional argument, and any later named arguments stay named.

Function values keep the labels from the callable surface you give them. An unlabeled callable annotation gives callers only positional access. A labeled callable annotation gives callers those labels.

The chapter example above shows the same function called positionally and with labels.

## What Musi does not have

Musi does not use a separate external-label system like some languages do.
The parameter names themselves are the labels.
That keeps the model smaller.

## Try it next

- Define one two-argument function.
- Call it positionally once.
- Call it again with named arguments in a different order.

## Common mistake

Do not think of parameter names as throwaway implementation detail if other modules call the function with labels.
When that happens, the names become part of the function's user-facing shape.

## Next

Continue to [Lambdas](/docs/language/core/lambdas) to write small function values inside expressions.
