---
title: "Functions"
description: "Define reusable functions, learn named arguments, and understand why parameter names matter."
group: "Core Syntax"
section: "Core Syntax"
order: 9
slug: "functions"
summary: "Functions are ordinary `let` bindings with parameters, result types, and expression bodies."
---

A function is a named calculation. It receives parameters, uses them in a body, and produces the final expression. Named arguments keep calls readable when values could be confused.

{{snippet:chapter-functions}}

{{snippet:named-callable-values}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Lambdas](/learn/book/core/functions-and-calls/lambdas).
