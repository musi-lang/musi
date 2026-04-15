---
title: "Variables and Mutation"
description: "Translate C# locals, var, and assignment into Musi names, mut values, and fresh bindings."
group: "Musi for Developers"
section: "C# Developers"
order: 4
slug: "variables-mutation"
summary: "Use mut for real state changes and fresh names for ordinary derived values."
---

C# local variables can be reassigned:

```csharp
var visits = 0;
visits += 1;

var nextVisits = visits + 1;
nextVisits;
```

Musi marks the value that can change with `mut`.

{{snippet:csharp-variables-mutation}}

Use `mut` like a visible dial on a machine. Readers know which dial can move.

## Fresh values

C# often uses `var` for derived values:

```csharp
var basePrice = 1200;
var total = basePrice + 45;
total;
```

Musi fresh bindings read the same way.

{{snippet:csharp-fresh-value}}

Use fresh names when a later value is another fact, not a state change.
