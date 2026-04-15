---
title: "Sealed Types and Patterns"
description: "Translate Java sealed interfaces and pattern matching into Musi data variants and match patterns."
group: "Musi for Developers"
section: "Java Developers"
order: 9
slug: "sealed-types-patterns"
summary: "Model alternatives with payload-carrying variants and inspect them with match."
---

# Sealed Types and Patterns

Java 17 sealed interfaces can model a closed set of alternatives:

```java
sealed interface TaskState permits Waiting, Running, Done {}
record Waiting() implements TaskState {}
record Running(int id) implements TaskState {}
record Done(int code) implements TaskState {}

TaskState state = new Running(42);
```

Musi variants put the closed alternatives and payloads in one data definition.

{{snippet:java-sealed-types-patterns}}

`match` makes each case visible at the use site.

