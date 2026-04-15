---
title: "Methods and Receiver Calls"
description: "Translate Java instance methods into Musi receiver-style calls."
group: "Musi for Developers"
section: "Java Developers"
order: 11
slug: "methods-and-receiver-calls"
summary: "Use receiver calls when the operation reads naturally from the left-hand value."
---

# Methods and Receiver Calls

Java puts the receiver before the dot:

```java
record Port(int value) {
    int withOffset(int offset) {
        return value + offset;
    }
}

int publicPort = new Port(8080).withOffset(1);
```

Musi can use receiver-style calls for operations that read naturally from the left-hand value.

{{snippet:java-methods-receiver-calls}}

Use ordinary functions when several inputs are equally important. Use receiver calls when the left-hand value is the thing being transformed.

