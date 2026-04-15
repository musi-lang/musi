---
title: "Blocks, Branching, and Loops"
description: "Translate Go block statements, switches, and loops into Musi block expressions, match, recursion, and pipelines."
group: "Musi for Developers"
section: "Go Developers"
order: 3
slug: "blocks-branching-loops"
summary: "Use blocks as values and model repetition with recursion or library traversal."
---

# Blocks, Branching, and Loops

Go blocks group statements and return explicitly when a value leaves a function:

```go
func invoiceTotal() int {
    basePrice := 1200
    fee := 45
    return basePrice + fee
}
```

Musi blocks can produce the final value directly.

{{snippet:go-block-expression}}

Go loops commonly mutate loop state:

```go
func totalSeats(groups int) int {
    seats := 0
    for remaining := groups; remaining > 0; remaining -= 1 {
        seats += 4
    }
    return seats
}
```

Musi uses recursion when the loop is a small state machine.

{{snippet:go-recursive-control-flow}}

For collections, prefer library traversal and pipelines. For changing counters or accumulators, make the state explicit in recursive parameters.
