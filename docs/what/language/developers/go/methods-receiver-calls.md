---
title: "Methods and Receiver Calls"
description: "Translate Go methods into Musi receiver-style calls."
group: "Musi for Developers"
section: "Go Developers"
order: 12
slug: "methods-receiver-calls"
summary: "Use receiver calls when the operation reads naturally from the left-hand value."
---

# Methods and Receiver Calls

Go methods place the receiver before the function name:

```go
type Port int

func (port Port) WithOffset(offset int) Port {
    return port + Port(offset)
}

publicPort := Port(8080).WithOffset(1)
```

Musi receiver methods keep the transformed value on the left at the call site.

{{snippet:go-methods-receiver-calls}}

Use ordinary functions when several inputs are equally important. Use receiver calls when the left-hand value is the thing being transformed.
