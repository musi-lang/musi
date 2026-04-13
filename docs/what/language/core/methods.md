---
title: "Methods"
description: "Learn Musi’s attached-method model after plain functions and calls."
group: "Core syntax"
section: "Core syntax"
order: 11
slug: "methods"
summary: "Use receiver-prefixed methods and dot calls without needing an impl block."
---

{{snippet:receiver-method}}

{{snippet:receiver-method-call}}

## What

Attached methods use receiver-prefixed declarations such as `let (self : Int).abs () := ...`.

## Why

This keeps attachment explicit without adding an `impl` wrapper.

## How

- Declare the receiver first.
- Put the method name after the dot in the declaration head.
- Call it with dot syntax.

## Try it

- Define one attached method.
- Call it with dot syntax.
- Compare it with an equivalent free function.

## Common mistake

Do not assume dot call falls back to a free function.

## Next

Continue to [Records](/docs/language/data/records).
