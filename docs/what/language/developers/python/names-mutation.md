---
title: "Names, Mutation, and Fresh Values"
description: "Translate Python rebinding and mutable habits into Musi mutable values and fresh bindings."
group: "Musi for Developers"
section: "Python Developers"
order: 4
slug: "names-mutation"
summary: "Use mut for real state changes and fresh names for ordinary derived values."
---

Python lets a name point at a new value later:

```python
visits = 0
visits = visits + 1

next_visits = visits + 1
next_visits
```

Musi makes real mutation visible at the value that can change.

{{snippet:python-names-mutation}}

Think of `mut` as a counter on a desk. You can change the number on that counter, but readers can see which counter is allowed to move.

## Fresh names first

When a step is just a derived value, prefer a new name:

```python
base = 1200
total = base + 45
total
```

Musi reads that style directly.

{{snippet:python-fresh-value}}

Use mutation for counters, cursors, buffers, and small accumulators. Use fresh names when the next value is another fact in the calculation.
