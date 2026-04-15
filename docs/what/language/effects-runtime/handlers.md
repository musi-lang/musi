---
title: "Handlers"
description: "Handle effects after the effect and using model are already clear."
group: "Effects and runtime"
section: "Effects and runtime"
order: 26
slug: "handlers"
summary: "Resolve requested effects at the boundary where policy belongs."
---

{{snippet:chapter-handlers}}

## In this chapter

A handler decides what to do with requested operations.
This example handles a console read request, gives a value path for normal completion, and uses `resume` to continue computation after operation match is handled.
It is first place where effect requests meet concrete policy.

## Why it matters

Users ask "where does the effect actually get answered?" right after they see `request`.
Handlers are that answer, but they are easier to learn once effect and `using` ideas are already clear.
A small handler example keeps focus on resolution flow instead of broad control abstractions.

## Walk through it

Read `handle ... using console` as boundary around effectful computation.
Inside the handler, `value => value;` covers normal completion, while `readln(k) => resume "ok";` covers specific requested operation and chooses how computation continues.
If a handler arm does not resume, the captured continuation does not continue from that request.
When writing your own handlers, start with one operation and one simple resume path so control flow stays easy to trace.

## Try it next

- Wrap one requested operation in a handler.
- Add one operation case.
- Resume computation with a concrete replacement value.

## Common mistake

Do not mix several effects and several policies into first handler example.

## Next

Continue to [Foundation](/docs/language/effects-runtime/foundation) to separate language-level core from runtime and stdlib layers built above it.
