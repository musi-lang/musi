---
title: "Blocks and Control Flow"
description: "Translate C++17 blocks, branching, switches, and loops into Musi block expressions, match, and recursion."
group: "Musi for Developers"
section: "C++ Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use blocks as values and model small state-machine loops with recursion."
---

# Blocks and Control Flow

C++17 blocks group statements and return explicitly when a value leaves a function:

```cpp
auto invoice_total() -> int {
    const auto base_price = 1200;
    const auto fee = 45;
    return base_price + fee;
}
```

Musi blocks can produce the final value directly.

{{snippet:cpp17-block-expression}}

C++ loops commonly mutate loop state:

```cpp
auto total_seats(const std::vector<int>& groups) -> int {
    auto seats = 0;
    for (const auto group : groups) {
        seats += group * 4;
    }
    return seats;
}
```

Musi uses recursion when the loop is a small state machine.

{{snippet:cpp17-recursive-control-flow}}

For collections, prefer library traversal and pipelines. For changing counters or accumulators, make state explicit in recursive parameters.
