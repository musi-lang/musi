---
title: "Blocks and Control Flow"
description: "Translate C99 blocks, branch statements, and loops into Musi block expressions, match, and recursion."
group: "Musi for Developers"
section: "C Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use blocks as values and model small state-machine loops with recursion."
---

# Blocks and Control Flow

C99 blocks group statements and return explicitly when a value leaves a function:

```c
int invoice_total(void) {
    int base_price = 1200;
    int fee = 45;
    return base_price + fee;
}
```

Musi blocks can produce the final value directly.

{{snippet:c99-block-expression}}

C loops commonly mutate loop state:

```c
int total_seats(int groups) {
    int seats = 0;
    for (int remaining = groups; remaining > 0; remaining -= 1) {
        seats += 4;
    }
    return seats;
}
```

Musi uses recursion when the loop is a small state machine.

{{snippet:c99-recursive-control-flow}}

For collections, prefer library traversal and pipelines. For changing counters or accumulators, make state explicit in recursive parameters.
