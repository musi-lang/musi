---
title: "Unions and Pattern Matching"
description: "Translate C# switch expressions and inheritance-based cases into Musi data variants and match arms."
group: "Musi for Developers"
section: "C# Developers"
order: 9
slug: "unions-pattern-matching"
summary: "Use data variants when each case has a name and a different payload."
---

C# often models cases with a base type and derived records:

```csharp
abstract record TaskState;
sealed record Waiting() : TaskState;
sealed record Running(int Id) : TaskState;
sealed record Done(int Code) : TaskState;

TaskState state = new Running(42);
var selected = state switch
{
    Running(var id) => id,
    Waiting => 0,
    Done(var code) => code,
};
```

Musi names those cases directly in one `data` definition.

{{snippet:csharp-unions-pattern-matching}}

Use variants when the set of cases matters more than object inheritance.
