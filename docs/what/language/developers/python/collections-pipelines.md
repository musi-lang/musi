---
title: "Collections and Pipelines"
description: "Translate Python list operations into Musi arrays, stdlib helpers, and pipelines."
group: "Musi for Developers"
section: "Python Developers"
order: 6
slug: "collections-pipelines"
summary: "Use arrays and pipeline-first stdlib helpers for visible collection flow."
---

Python list code often grows by applying one collection operation after another:

```python
ports = [3000, 8080]
visible = [*ports, 9000]
visible
```

Musi arrays use `[]`. The pipeline operator puts the value being transformed on the left and the operation on the right.

{{snippet:python-collections-pipelines}}

Read the pipeline like a conveyor belt: `ports` enters, `iter.append` adds a port, and `iter.collect` leaves an array for the next step.

## Why this matters for Python readers

Python list comprehensions are compact. Musi favors a visible chain when multiple steps matter. That keeps later additions easy to review.
