---
title: "None, Option, and Result"
description: "Translate Python None and ordinary failure values into Musi Option and Result."
group: "Musi for Developers"
section: "Python Developers"
order: 7
slug: "none-option-result"
summary: "Use Option for maybe-present values and Result for ordinary success-or-error data."
---

Python commonly uses `None` when a value might be missing:

```python
def lookup_port(name: str) -> int | None:
    match name:
        case "admin":
            return 9000
        case _:
            return None

port = lookup_port("web") or 8080
port
```

Musi uses `Option` so absence is visible in the type and at the call site.

{{snippet:python-none-option}}

The caller decides the fallback. The function only reports whether it found a value.

## Result for ordinary failure

When Python returns a value or raises for ordinary input failure, Musi can model that as data.

{{snippet:python-result-value}}

Use `Option` for missing value. Use `Result` when the error carries information a caller may read.
