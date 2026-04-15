---
title: "Overview"
description: "Translate Java 17 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "Java Developers"
order: 1
slug: "overview"
summary: "Start from Java 17 habits, then read equivalent Musi expression, data, effect, and package shapes."
---

# Musi for Java Developers

Java 17 is the comparison point for this guide. Java developers often bring classes, records, sealed interfaces, exceptions, streams, packages, modules, annotations, and JNI boundaries. Musi keeps those jobs recognizable while making values, effects, and data shape explicit.

Java usually puts small calculations inside methods:

```java
static int total(int basePrice, int fee) {
    return basePrice + fee;
}
```

Musi keeps the same input and output types, but the body is the produced value.

{{snippet:java-values-methods-expressions}}

## Reading path

The guide moves from everyday Java code toward deeper language design:

1. values, methods, and expressions;
2. blocks, control flow, variables, and mutation;
3. records, sealed types, collections, null, results, and effects;
4. generics, interfaces, packages, tests, native boundaries, and unsafe code.

