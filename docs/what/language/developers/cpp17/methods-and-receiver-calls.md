---
title: "Methods and Receiver Calls"
description: "Translate C++17 methods into Musi functions and receiver-style calls where they improve reading."
group: "Musi for Developers"
section: "C++ Developers"
order: 11
slug: "methods-and-receiver-calls"
summary: "Keep functions primary and use receiver calls for left-to-right transformations."
---

# Methods and Receiver Calls

C++17 attaches receiver behavior to a class or struct:

```cpp
struct Port {
    int port;

    auto with_offset(const int offset) const -> int {
        return port + offset;
    }
};

const auto selected = Port{8080}.with_offset(100);
```

Musi keeps functions primary while allowing receiver-style calls.

{{snippet:cpp17-methods-receiver-calls}}
