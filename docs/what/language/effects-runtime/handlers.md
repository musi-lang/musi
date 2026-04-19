---
title: "Handlers"
description: "Handle effects after the effect and using model are already clear."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 27
slug: "handlers"
summary: "Resolve requested effects at the boundary where policy belongs."
---
Handlers answer effect requests. If effects are service desks, handlers are the people behind the counters deciding what answer to give.

{{snippet:chapter-handlers}}

A handler can talk to the real runtime, return fixed test data, or translate one effect into another. The calling code does not need to know which strategy was chosen.

## Real handlers and test handlers

Production code may answer `Clock.now` with the machine's current time. A test can answer the same request with Monday morning every time. That keeps the rule being tested stable while still using the same effect shape.

## Keep handler policy clear

Handlers are powerful because they sit at the boundary. Keep policy visible there. A payment handler that retries failed network calls should make that retry rule easy to find, not hide it behind unrelated formatting code.

Effect chapters draw a line between local calculation and outside answers. Adding prices is local. Reading time, asking a process, or writing a log needs an answer from outside the expression.

A useful test is to ask whether the result could be known from the input alone. If yes, keep the function plain. If no, name the request and make the effect boundary visible.
