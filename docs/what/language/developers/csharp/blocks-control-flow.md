---
title: "Blocks and Control Flow"
description: "Translate C# statement blocks, switch expressions, and loops into Musi blocks, match arms, and recursion."
group: "Musi for Developers"
section: "C# Developers"
order: 3
slug: "blocks-control-flow"
summary: "Use expression blocks, match arms, and let rec for repeated work."
---

C# statement blocks often collect local setup before a `return`:

```csharp
static int InvoiceTotal()
{
    var basePrice = 1200;
    var fee = 45;
    return basePrice + fee;
}

InvoiceTotal();
```

Musi blocks produce a value. The final expression is what leaves the block.

{{snippet:csharp-block-expression}}

Think of the block as a workbench. Local names stay on the bench; the final expression is the finished part.

## Repetition

C# loops can update local state:

```csharp
static int TotalSeats(int rounds)
{
    var seats = 0;
    for (var remaining = rounds; remaining > 0; remaining -= 1)
    {
        seats += 4;
    }
    return seats;
}
```

Musi gives repeated work a name with `let rec`.

{{snippet:csharp-recursive-control-flow}}

The `match` arms describe the two roads: stop, or continue with less remaining work.
