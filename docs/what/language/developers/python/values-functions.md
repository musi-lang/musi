---
title: "Values, Functions, and Final Expressions"
description: "Translate Python def, annotations, and return into Musi let functions and final expressions."
group: "Musi for Developers"
section: "Python Developers"
order: 2
slug: "values-functions"
summary: "Use let functions and final expressions where Python uses def and return."
---

Python `def` introduces a function. A type annotation helps readers and tools, but `return` still marks the value that leaves the function.

```python
def total(base: int, fee: int) -> int:
    return base + fee

answer = total(1200, 45)
answer
```

Musi uses `let` for named values and named functions. The function body can be one expression.

{{snippet:python-values-functions}}

The important habit change is where the result sits. Python says `return base + fee`; Musi lets `base + fee` be the body itself.

## Named calls

Python keyword arguments help a call site read like a form:

```python
def render(port: int, secure: bool) -> int:
    return port

selected = render(port=8080, secure=True)
selected
```

Musi supports the same readability with named arguments.

{{snippet:python-named-calls}}

Use named calls when position alone would make the call hard to read.
