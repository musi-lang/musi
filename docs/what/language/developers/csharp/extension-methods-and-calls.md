---
title: "Extension Methods and Calls"
description: "Translate C# extension method call habits into Musi receiver methods and ordinary calls."
group: "Musi for Developers"
section: "C# Developers"
order: 11
slug: "extension-methods-and-calls"
summary: "Use receiver methods when a call should read from the value outward."
---

C# extension methods let a static function read like a method on the receiver:

```csharp
public static class PortExtensions
{
    public static int WithOffset(this int port, int offset) => port + offset;
}

var publicPort = 8080.WithOffset(1);
publicPort;
```

Musi receiver methods serve the same reading goal.

{{snippet:csharp-extension-methods-and-calls}}

Use a receiver method when the left-side value is the thing being described.

## Ordinary calls still fit

Not every helper should be a method. If the operation belongs to a module or package rather than one receiver value, keep it as a plain function call.
