---
title: "Musi for JavaScript Developers"
description: "Learn how JavaScript habits translate into Musi code."
group: "Musi for Developers"
section: "Musi for Developers"
order: 1
slug: "musi-for-javascript-developers"
summary: "Start from JavaScript habits and write the same ideas as Musi expressions."
---

{{snippet:guide-javascript-developers}}

JavaScript developers often bring objects, functions, promises, and module imports.
Musi keeps the useful part of that experience, but the code shape changes: Use records for plain object shape, functions as values, packages for modules, and effects for async-like boundary work.

## First Translation

Read the example as one small program.
A `let` binding names a value or function.
A block ends with the value it produces.
A `match` expression returns a value from the selected arm.

## Habits That Transfer

- Name data by domain, not by container.
- Keep side effects at boundaries.
- Prefer small functions with explicit inputs.
- Put package boundaries where readers need names to stop leaking.

## Habits to Relearn

- Do not write a `return` keyword for the final value.
- Do not model every concept as an object.
- Do not hide boundary work; use `request`, `using`, handlers, or `unsafe` where the operation crosses a line.

## Where to Go Next

- [Values and Let](/learn/book/start/values-and-let) for binding rules.
- [Functions](/learn/book/core/functions) for named parameters and calls.
- [Data Definitions](/learn/book/data/data-definitions) for variant payloads.
- [Effects](/learn/book/effects-runtime/effects) for requested work.
