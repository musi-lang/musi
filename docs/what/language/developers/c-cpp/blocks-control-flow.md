---
title: "Blocks and Control Flow"
description: "Translate statement blocks and loops into Musi block expressions, match, recursion, and pipelines."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use blocks as values and model repetition with recursion or library traversal."
---

# Blocks and Control Flow

C and C++ blocks group statements. Musi blocks group declarations and produce a final value. This makes temporary setup code easy to keep local without inventing a mutable output variable.

C often names a temporary, assigns it, then returns it:

```c
int invoice_total(void) {
    int base_price = 1200;
    int fee = 45;
    int total = base_price + fee;
    return total;
}
```

C++ can compress that with an immediately invoked lambda, but it is still extra ceremony for a local value:

```cpp
auto invoice_total = []() -> int {
    auto base_price = 1200;
    auto fee = 45;
    return base_price + fee;
};
```

Musi lets the block itself produce the final value.

{{snippet:c-cpp-block-expression}}

The block creates two local values and returns the final expression. No `return` is needed inside the block.

## Repetition uses recursion and library traversal

C and C++ often reach for `for`, `while`, and `do` loops. Musi keeps explicit recursion visible with `let rec`, and common collection work should use library traversal and pipelines.

C loop state is usually mutation:

```c
int total_seats(int groups) {
    int seats = 0;
    for (int remaining = groups; remaining > 0; remaining -= 1) {
        seats += 4;
    }
    return seats;
}
```

C++ can write the same state with a loop:

```cpp
auto total_seats(int groups) -> int {
    auto seats = 0;
    for (auto remaining = groups; remaining > 0; --remaining) {
        seats += 4;
    }
    return seats;
}
```

Musi names the state that changes across recursive calls.

{{snippet:c-cpp-recursive-control-flow}}

Read this like a small state machine:

- `remaining` says how many groups still need seats.
- `seats` carries the accumulated count.
- `match` handles the stopping case and the continuing case.

For collection-heavy code, prefer pipelines from `@std/iter` over hand-written recursive traversal.
