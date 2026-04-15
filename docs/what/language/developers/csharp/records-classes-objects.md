---
title: "Records, Classes, and Objects"
description: "Translate C# records, classes, and object updates into Musi data definitions and records."
group: "Musi for Developers"
section: "C# Developers"
order: 5
slug: "records-classes-objects"
summary: "Use data definitions for named shapes and records for named field values."
---

C# records give a named shape and a copy-update form:

```csharp
public sealed record Endpoint(string Host, int Port, bool Secure);

var local = new Endpoint("localhost", 8080, false);
var publicEndpoint = local with { Host = "api.example.com", Secure = true };
publicEndpoint.Port;
```

Musi names the shape with `data` and updates field values with record spread.

{{snippet:csharp-records-classes-objects}}

Use records like a badge: each field name says what role the value plays.

## Class habit

C# classes often mix identity, mutation, and behavior. In Musi, choose the smallest shape that matches the job: a record for fields, a `data` variant for cases, and a class plus instance for shared behavior.
