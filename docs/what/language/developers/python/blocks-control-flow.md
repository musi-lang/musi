---
title: "Blocks, Control Flow, and Repetition"
description: "Translate Python indentation, loops, and branching habits into Musi expression blocks and recursion."
group: "Musi for Developers"
section: "Python Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use blocks as expressions and let rec when repeated work needs a named path."
---

Python indentation defines a block. A loop changes local state as it repeats work:

```python
def total_seats(rounds: int) -> int:
    seats = 0
    remaining = rounds
    while remaining > 0:
        seats = seats + 4
        remaining = remaining - 1
    return seats

total_seats(3)
```

Musi gives the repeated path a name. `let rec` is the road sign that says the function may call itself.

{{snippet:python-blocks-control-flow}}

Read the recursive version like a delivery route. One arm says the route is finished. The other arm makes the next stop with smaller remaining work.

## Blocks return values

Python often builds a local value and returns it at the end of the function. Musi blocks can do the same job as one expression.

{{snippet:python-block-expression}}

Use this when a calculation needs local names but outside code should see only the finished value.
