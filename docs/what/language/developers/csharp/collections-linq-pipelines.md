---
title: "Collections, LINQ, and Pipelines"
description: "Translate C# collection and LINQ habits into Musi arrays, stdlib helpers, and pipelines."
group: "Musi for Developers"
section: "C# Developers"
order: 6
slug: "collections-linq-pipelines"
summary: "Use arrays and pipeline-first stdlib helpers for visible collection flow."
---

C# 12.0 collection expressions make small arrays direct:

```csharp
int[] ports = [3000, 8080];
int[] visible = [.. ports, 9000];
visible;
```

Musi arrays use `[]`, and pipelines keep transformations left to right.

{{snippet:csharp-collections-linq-pipelines}}

Read the pipeline like a conveyor: the collection enters on the left, each helper does one step, and the final value leaves on the right.

## LINQ habit

LINQ chains are useful because readers can follow each operation. Musi pipelines keep that same shape, with the data flow visible at the beginning of each line.
