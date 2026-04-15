---
title: "Variables and Mutation"
description: "Translate Java reassignment into explicit Musi mutation and fresh values."
group: "Musi for Developers"
section: "Java Developers"
order: 4
slug: "variables-mutation"
summary: "Use mutable bindings for real changing state and fresh names for calculation stages."
---

# Variables and Mutation

Java local variables can be reassigned unless they are `final`:

```java
int visits = 0;
visits = visits + 1;

int nextVisits = visits + 1;
```

Musi marks the binding that can change.

{{snippet:java-variables-mutation}}

## Fresh values

Java developers often use `final` to make calculation stages stable:

```java
final int basePrice = 1200;
final int total = basePrice + 45;
```

Musi bindings are stable by default, so fresh names fit ordinary calculations.

{{snippet:java-fresh-value}}

