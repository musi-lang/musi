---
title: "Handlers"
description: "Handle effects after the effect and using model are already clear."
group: "Effects and runtime"
section: "Effects and runtime"
order: 26
slug: "handlers"
summary: "Resolve performed effects at the boundary where policy belongs."
---

{{snippet:handle-console}}

## What

A handler decides what to do when effect operations are performed.

## Why

This keeps core logic separate from boundary policy.

## How

- Wrap effectful work in `handle ... using ...`.
- Provide a value clause.
- Provide the effect clauses you need.

## Try it

- Perform one effect operation.
- Handle it once.
- Change the handled result.

## Common mistake

Do not push all business logic into handlers.

## Next

Continue to [Foundation](/docs/language/effects-runtime/foundation).
