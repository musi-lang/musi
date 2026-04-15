---
title: "Exceptions, Results, and Effects"
description: "Translate C# exceptions, async tasks, and outside work into Musi Result values and effectful boundaries."
group: "Musi for Developers"
section: "C# Developers"
order: 8
slug: "exceptions-effects"
summary: "Use Result for recoverable failure and effects or stdlib helpers for requested outside work."
---

C# exceptions can move failure out of the return path:

```csharp
static int ParsePort(string text)
{
    try
    {
        return int.Parse(text);
    }
    catch (FormatException)
    {
        return 3000;
    }
}
```

Musi keeps ordinary failure in a `Result` value.

{{snippet:csharp-exceptions-results}}

Use this when bad input is part of normal use, not a crash.

## Async and outside work

C# `Task`, `async`, I/O, timers, and process calls reach outside a pure calculation. Musi keeps those edges visible through stdlib modules and effects.

{{snippet:csharp-effect-boundary}}

The import names the outside capability. The call site shows where input and output happen.
