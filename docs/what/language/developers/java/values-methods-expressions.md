---
title: "Values, Methods, and Expressions"
description: "Translate Java methods and expression habits into Musi expression-bodied functions and named calls."
group: "Musi for Developers"
section: "Java Developers"
order: 2
slug: "values-methods-expressions"
summary: "Use expression bodies, positional calls, and named calls for readable functions."
---

# Values, Methods, and Expressions

Java methods usually wrap work in a statement body:

```java
static int total(int basePrice, int fee) {
    return basePrice + fee;
}

int answer = total(1200, 45);
```

Musi functions name parameters and output type directly, and the body expression is the result.

{{snippet:java-values-methods-expressions}}

## Named calls

Java call sites are positional:

```java
int selected = render(8080, true);
```

That is compact, but numbers and booleans can become hard to read. Musi supports named calls when labels help.

{{snippet:java-named-calls}}

