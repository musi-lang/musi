---
title: "Blocks and Control Flow"
description: "Translate Java statement blocks and loops into Musi block expressions, match, recursion, and pipelines."
group: "Musi for Developers"
section: "Java Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use blocks as values and model repetition with recursion or library traversal."
---

# Blocks and Control Flow

Java blocks group statements and usually require `return` for produced values:

```java
static int invoiceTotal() {
    int basePrice = 1200;
    int fee = 45;
    return basePrice + fee;
}
```

Musi blocks can produce the final value directly.

{{snippet:java-block-expression}}

## Repetition

Java loop state is commonly mutated:

```java
static int totalSeats(int groups) {
    int seats = 0;
    for (int remaining = groups; remaining > 0; remaining -= 1) {
        seats += 4;
    }
    return seats;
}
```

Musi can make the changing state visible through recursive parameters.

{{snippet:java-recursive-control-flow}}

Use library traversal and pipelines for collection work. Use `let rec` when the loop is really a small state machine.

