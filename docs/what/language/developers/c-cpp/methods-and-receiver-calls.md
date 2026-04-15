---
title: "Methods and Receiver Calls"
description: "Translate C receiver parameters and C++ member calls into Musi receiver-style calls."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 11
slug: "methods-and-receiver-calls"
summary: "Use receiver calls when the operation reads naturally from the left-hand value."
---

# Methods and Receiver Calls

C passes the receiver explicitly:

```c
int with_offset(int port, int offset) {
    return port + offset;
}

int public_port = with_offset(8080, 1);
```

C++ usually puts the receiver before the dot:

```cpp
struct Port {
    int value;

    auto with_offset(int offset) const -> int {
        return value + offset;
    }
};

auto public_port = Port{8080}.with_offset(1);
```

Musi supports receiver-style calls when they make a transformation read naturally.

{{snippet:c-cpp-methods-receiver-calls}}

Use receiver calls for operations that feel attached to the left-hand value. Use ordinary function calls when the operation combines several independent values.
