---
title: "Data Variants and Pattern Matching"
description: "Translate Python match and tagged object habits into Musi data variants and match arms."
group: "Musi for Developers"
section: "Python Developers"
order: 9
slug: "data-variants-patterns"
summary: "Use data variants when each case has a name and a different payload."
---

Python `match` can branch on object shape, strings, tuples, or classes:

```python
state = {"kind": "running", "id": 42}

match state:
    case {"kind": "running", "id": task_id}:
        selected = task_id
    case {"kind": "waiting"}:
        selected = 0
    case _:
        selected = -1

selected
```

Musi names the cases directly with `data` variants. The payload label says what travels with that case.

{{snippet:python-data-variants-patterns}}

Use variants when a plain dictionary would make readers remember which string tags and fields are valid.
