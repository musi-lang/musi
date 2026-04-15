---
title: "Values, Methods, and Expressions"
description: "Translate C# methods, expression-bodied members, and return into Musi let functions and final expressions."
group: "Musi for Developers"
section: "C# Developers"
order: 2
slug: "values-methods-expressions"
summary: "Use let functions and final expressions where C# uses methods and return."
---

C# methods name work. Expression-bodied methods make a simple calculation compact:

```csharp
static int Total(int basePrice, int fee) => basePrice + fee;

var answer = Total(1200, 45);
answer;
```

Musi uses `let` to name functions. The body can be the expression itself.

{{snippet:csharp-values-methods-expressions}}

No `return` is needed for this shape. The expression after `:=` is the value.

## Named arguments

C# named arguments make call sites clearer:

```csharp
static int Render(int port, bool secure) => port;

var selected = Render(port: 8080, secure: true);
selected;
```

Musi uses named arguments for the same reading job.

{{snippet:csharp-named-calls}}

Use named calls when two adjacent values could be confused.
