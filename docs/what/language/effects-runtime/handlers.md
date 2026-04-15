---
title: "Handlers"
description: "Handle effects after the effect and using model are already clear."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 26
slug: "handlers"
summary: "Resolve requested effects at the boundary where policy belongs."
---

A handler interprets requested operations. It decides what a request means at a particular boundary.

{{snippet:chapter-handlers}}

Read `handle Clock.tick() using Clock { ... }` as running an expression while interpreting clock requests with this handler. `value => value;` covers normal completion. `tick(k) => resume 1;` answers one requested operation and continues the suspended computation.

A handler is a local policy desk: request asks for help; handler says what help means here.

Continue to [Foundation](/learn/book/effects-runtime/runtime-model/foundation).
