---
title: "Testing and Tooling"
description: "Translate C99 test harness conventions into Musi package tests and std testing helpers."
group: "Musi for Developers"
section: "C Developers"
order: 13
slug: "testing-tooling"
summary: "Keep tests beside the package and run them through musi test."
---

# Testing and Tooling

C99 tests often call ordinary functions from a test harness or `assert`:

```c
#include <assert.h>

int total(int base_price, int fee) {
    return base_price + fee;
}

void test_total(void) {
    assert(total(1200, 45) == 1245);
}
```

Musi keeps checks beside the package with `@std/testing`.

{{snippet:c99-testing-tooling}}
