---
title: "Exceptions and Effects"
description: "Translate Java exceptions and host interaction into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "Java Developers"
order: 8
slug: "exceptions-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Exceptions and Effects

Java exceptions move failure through control flow:

```java
static int parsePort(String text) {
    if (text.equals("8080")) {
        return 8080;
    }
    throw new IllegalArgumentException("parse error");
}
```

Musi keeps expected failure in the returned value.

{{snippet:java-exceptions-results}}

## Outside work

Java host interaction usually calls library APIs directly:

```java
var scanner = new Scanner(System.in);
String name = scanner.nextLine().trim();
System.out.println(name);
```

Musi keeps console work at effectful boundaries.

{{snippet:java-effect-boundary}}

