---
title: "Collections, Streams, and Pipelines"
description: "Translate Java collections and streams into Musi collection pipelines."
group: "Musi for Developers"
section: "Java Developers"
order: 6
slug: "collections-streams-pipelines"
summary: "Use pipelines for collection transformations that should read left to right."
---

# Collections, Streams, and Pipelines

Java collections often combine a collection type with mutation or streams:

```java
var ports = new ArrayList<Integer>(List.of(3000, 8080));
ports.add(9000);
var visible = List.copyOf(ports);
```

Streams make transformations read more declaratively:

```java
var visible = Stream.concat(Stream.of(3000, 8080), Stream.of(9000))
    .toList();
```

Musi pipelines keep the value moving left to right.

{{snippet:java-collections-streams-pipelines}}

