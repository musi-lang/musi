---
title: "Generics, Interfaces, and Laws"
description: "Translate C# generics, interfaces, and constraints into Musi type parameters, classes, instances, and laws."
group: "Musi for Developers"
section: "C# Developers"
order: 10
slug: "generics-interfaces-laws"
summary: "Use generics for reusable shape and classes with laws for shared behavior with rules."
---

C# generics let one function keep the same shape for many types:

```csharp
static T Identity<T>(T input) => input;

var port = Identity<int>(8080);
port;
```

Musi writes type parameters beside the function name.

{{snippet:csharp-generic-function}}

The function promises to return the same type it receives.

## Interfaces and laws

C# interfaces name behavior. Musi classes name behavior too, and laws can name rules that valid instances should satisfy.

```csharp
public interface IVehicle
{
    int Wheels { get; }
}
```

Musi separates the behavior from the instance that provides it.

{{snippet:csharp-interface-class-law}}

A car is a vehicle, and the law says what this model expects from valid vehicle instances.
