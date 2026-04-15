---
title: "Generics, Interfaces, and Laws"
description: "Translate Java generics and interfaces into Musi type parameters, classes, instances, and laws."
group: "Musi for Developers"
section: "Java Developers"
order: 10
slug: "generics-interfaces-laws"
summary: "Use generic functions for reuse and classes with laws for behavior contracts."
---

# Generics, Interfaces, and Laws

Java generics make reusable functions and types explicit:

```java
static <T> T identity(T input) {
    return input;
}

int port = identity(8080);
```

Musi uses type parameters for the same reusable shape.

{{snippet:java-generic-function}}

## Interfaces and laws

Java interfaces describe required behavior:

```java
interface Vehicle {
    int wheels();
}

record Car() implements Vehicle {
    public int wheels() {
        return 4;
    }
}
```

Musi classes state required operations, and laws record behavior expectations that implementations should satisfy.

{{snippet:java-interface-class-law}}

