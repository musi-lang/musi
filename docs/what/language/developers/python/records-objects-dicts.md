---
title: "Records, Objects, and Dictionaries"
description: "Translate Python dictionaries and dataclass-style data into Musi records and data definitions."
group: "Musi for Developers"
section: "Python Developers"
order: 5
slug: "records-objects-dicts"
summary: "Use records for named fields and data definitions when the shape deserves a name."
---

Python dictionaries are quick named bags of data:

```python
local = {
    "host": "localhost",
    "port": 8080,
    "secure": False,
}

public = {**local, "host": "api.example.com", "secure": True}
public["host"]
```

Musi records keep the same named-field idea. A `data` definition names the expected shape, and record spread updates selected fields.

{{snippet:python-records-objects-dicts}}

Use records like a shipping label: every field name tells the reader what the slot means.

## Dataclass habit

Python dataclasses give a stable shape to object-like data. In Musi, a `data` definition is the stable shape; the record literal is the value that fills it.
