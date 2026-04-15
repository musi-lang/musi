---
title: "Methods"
description: "Learn Musi's attached-method model after plain functions and calls."
group: "Core Syntax"
section: "Core Syntax"
order: 11
slug: "methods"
summary: "Use receiver-prefixed methods and dot calls without needing an impl block."
---

Methods are functions attached to a receiver shape. The receiver comes before the dot, which makes the main subject visible first.

{{snippet:chapter-methods}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Records](/learn/book/data/modeling/records).
