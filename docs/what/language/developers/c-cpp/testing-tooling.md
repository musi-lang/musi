---
title: "Testing and Tooling"
description: "Translate C and C++ test habits into small Musi package tests and toolchain checks."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 13
slug: "testing-tooling"
summary: "Write tests as small examples and use the package toolchain for routine checks."
---

# Testing and Tooling

C projects often combine compiler warnings, unit test frameworks, sanitizers, and scripts. C++ projects often add build-system targets and template-heavy compile checks. Musi tests should read like small package examples.

C can use a direct assertion for a tiny rule:

```c
#include <assert.h>

void default_port_is_8080(void) {
    assert(default_port() == 8080);
}
```

C++ can express the same check with a test framework or assertion:

```cpp
#include <cassert>

auto default_port_is_8080() -> void {
    assert(default_port() == 8080);
}
```

Musi tests keep the rule name, the small value, and the expectation together.

{{snippet:c-cpp-testing-tooling}}

Keep tests close to the behavior they explain. A good test names the rule, builds a small value, and checks the outcome.

Use the package toolchain for formatting, checking, and running tests so local work matches CI behavior.
