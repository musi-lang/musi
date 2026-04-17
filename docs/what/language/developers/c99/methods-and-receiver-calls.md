---
title: "Methods and Receiver Calls"
description: "Translate C99 subsystem-prefixed functions into Musi receiver-style calls where they improve reading."
group: "Musi for Developers"
section: "C Developers"
order: 11
slug: "methods-and-receiver-calls"
summary: "Keep functions primary and use receiver calls for left-to-right transformations."
---

# Methods and Receiver Calls

C99 keeps receiver-like behavior as a prefixed function:

```c
typedef struct {
    int port;
} Endpoint;

int endpoint_with_offset(Endpoint endpoint, int offset) {
    return endpoint.port + offset;
}

Endpoint endpoint = {8080};
int selected = endpoint_with_offset(endpoint, 100);
```

Musi keeps functions primary while allowing receiver-style calls.

{{snippet:c99-methods-receiver-calls}}
