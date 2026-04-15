---
title: "Null, Option, and Result"
description: "Translate C# nullable reference habits into Musi Option and Result values."
group: "Musi for Developers"
section: "C# Developers"
order: 7
slug: "null-option-result"
summary: "Use Option for maybe-present values and Result for success-or-error data."
---

C# nullable references make absence visible in the type:

```csharp
static int? LookupPort(string name) =>
    name == "admin" ? 9000 : null;

var port = LookupPort("web") ?? 8080;
port;
```

Musi uses `Option` to make the same absence explicit.

{{snippet:csharp-null-option}}

The function reports whether a value exists. The caller chooses the fallback.

## Result for ordinary failure

When a C# API would return a success value or an error value, Musi can use `Result`.

{{snippet:csharp-result-value}}

Use `Option` for maybe-present values. Use `Result` when the failure carries information.
