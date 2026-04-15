---
title: "Testing and Tooling"
description: "Translate C# test habits into Musi std testing helpers and command-line checks."
group: "Musi for Developers"
section: "C# Developers"
order: 13
slug: "testing-tooling"
summary: "Use @std/testing to keep tests close to behavior."
---

C# tests commonly name a behavior and assert the result:

```csharp
[Fact]
public void DefaultPortIsHttpAlt()
{
    Assert.Equal(8080, DefaultPort());
}
```

Musi uses `@std/testing` for named suites and cases.

{{snippet:csharp-testing-tooling}}

A test should read like a receipt: name the behavior, run the check, and leave a pass or fail value.

## Tooling habit

.NET projects commonly separate restore, build, test, and format steps. Musi projects should keep the same habit: use commands that make syntax, package shape, and test behavior visible.
