---
title: "Testing and Tooling"
description: "Translate Python test habits into Musi std testing helpers and command-line checks."
group: "Musi for Developers"
section: "Python Developers"
order: 12
slug: "testing-tooling"
summary: "Use @std/testing to keep tests close to the behavior they describe."
---

Python tests often name a small behavior and assert the result:

```python
def default_port() -> int:
    return 8080

def test_default_port() -> None:
    assert default_port() == 8080
```

Musi uses `@std/testing` for named test suites and cases.

{{snippet:python-testing-tooling}}

A test should read like a receipt: name the behavior, run the check, and leave a pass or fail value.

## Tooling habit

Python projects often separate formatting, type checks, and tests. Musi projects should do the same kind of loop with package checks and tests as the toolchain grows.
