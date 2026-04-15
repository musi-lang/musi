---
title: "Exceptions, Results, and Effects"
description: "Translate Python exceptions and outside work into Musi Result values and effectful boundaries."
group: "Musi for Developers"
section: "Python Developers"
order: 8
slug: "exceptions-effects"
summary: "Use Result for recoverable failure and effects or stdlib helpers for requested outside work."
---

Python exceptions can carry a failure out of the ordinary return path:

```python
def parse_port(text: str) -> int:
    try:
        return int(text)
    except ValueError:
        return 3000

port = parse_port("abc")
port
```

Musi keeps ordinary failure in a `Result` value. That makes the fallback decision visible.

{{snippet:python-exceptions-results}}

Use this when failure is a normal part of the domain: bad input, missing config, or a rejected request.

## Outside work

Python `input`, file access, time, and network calls reach outside the pure calculation. Musi keeps those edges explicit through stdlib modules and effects.

{{snippet:python-effect-boundary}}

The import tells readers where outside work comes from. The call site shows that the program is asking the runtime for input and output.
