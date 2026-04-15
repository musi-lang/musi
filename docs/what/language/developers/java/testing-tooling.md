---
title: "Testing and Tooling"
description: "Translate Java test habits into small Musi package tests and toolchain checks."
group: "Musi for Developers"
section: "Java Developers"
order: 13
slug: "testing-tooling"
summary: "Write tests as small examples and use the package toolchain for routine checks."
---

# Testing and Tooling

Java tests often name one rule and assert the expected value:

```java
@Test
void defaultPortIs8080() {
    assertEquals(8080, Ports.defaultPort());
}
```

Musi tests keep the rule name, the small value, and the expectation together.

{{snippet:java-testing-tooling}}

Use the package toolchain for formatting, checking, and running tests so local work matches CI behavior.

