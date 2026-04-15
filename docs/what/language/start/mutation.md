---
title: "Mutation"
description: "Learn Musi's explicit mutation surface without mixing it into every lesson."
group: "Start"
section: "Start"
order: 5
slug: "mutation"
summary: "Use mut only when changing a value helps more than rebuilding it."
---

Mutation is explicit and value-based. A binding can hold a mutable value, but ordinary `let` still means "name this value."

{{snippet:chapter-mutation}}

Read `let counter := mut 1;` as "counter starts with a mutable value." Read `counter := counter + 1;` as "replace the current value with the next value." The update is visible at the assignment site.

A real-world counter works the same way. The clicker is the same object in your hand, but its displayed number changes.

## Use It Sparingly

Mutation is useful for counters, cursors, buffers, and small accumulators. Prefer fresh names when the next value is just another step in a pipeline.

Continue to [Literals](/learn/book/core/expressions/literals).
