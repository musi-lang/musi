---
title: "Nil, Option, and Result"
description: "Translate Go nil checks and comma-ok patterns into Musi Option and Result values."
group: "Musi for Developers"
section: "Go Developers"
order: 7
slug: "nil-option-result"
summary: "Use Option for absence and Result for failure that carries information."
---

# Nil, Option, and Result

Go has `nil` for pointers, slices, maps, channels, functions, and interfaces. Map lookup also has a comma-ok form:

```go
ports := map[string]int{"admin": 9000}
port, ok := ports["web"]
if !ok {
    port = 8080
}
```

Musi uses the stdlib `Option` shape when a value may be absent.

{{snippet:go-nil-option}}

## Failure with information

Go commonly returns `(value, error)`:

```go
func parsePort(text string) (int, error) {
    if text == "8080" {
        return 8080, nil
    }
    return 0, errors.New("invalid port")
}
```

Musi uses `Result` when callers should receive either a value or an error.

{{snippet:go-result-value}}
