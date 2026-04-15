---
title: "Null, Option, and Result"
description: "Translate Java null, Optional, and expected failure into Musi Option and Result values."
group: "Musi for Developers"
section: "Java Developers"
order: 7
slug: "null-option-result"
summary: "Use Option for absence and Result for failure that carries information."
---

# Null, Option, and Result

Java has `null`, and Java 17 also has `Optional` for maybe-present values:

```java
static Optional<Integer> lookupPort(String name) {
    if (name.equals("admin")) {
        return Optional.of(9000);
    }
    return Optional.empty();
}

int port = lookupPort("web").orElse(8080);
```

Musi uses the stdlib `Option` shape for ordinary absence.

{{snippet:java-null-option}}

## Failure with information

Java often uses exceptions or small result classes when failure should carry information:

```java
record ParsePort(boolean ok, int port, String message) {}

static ParsePort parsePort(String text) {
    if (text.equals("8080")) {
        return new ParsePort(true, 8080, "");
    }
    return new ParsePort(false, 0, "invalid port");
}
```

Musi uses `Result` when callers should receive either a value or an error.

{{snippet:java-result-value}}

