---
title: "Testing and Tooling"
description: "Translate Go test habits into small Musi package tests and toolchain checks."
group: "Musi for Developers"
section: "Go Developers"
order: 14
slug: "testing-tooling"
summary: "Write tests as small examples and use the package toolchain for routine checks."
---

# Testing and Tooling

Go tests name one behavior and use `testing.T` for failure reporting:

```go
func TestDefaultPort(t *testing.T) {
    if DefaultPort() != 8080 {
        t.Fatalf("default port mismatch")
    }
}
```

Musi tests keep the rule name, the small value, and the expectation together.

{{snippet:go-testing-tooling}}

Use the package toolchain for formatting, checking, and running tests so local work matches CI behavior.
