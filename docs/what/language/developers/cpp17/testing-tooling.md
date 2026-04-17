---
title: "Testing and Tooling"
description: "Translate C++17 test framework conventions into Musi package tests and std testing helpers."
group: "Musi for Developers"
section: "C++ Developers"
order: 13
slug: "testing-tooling"
summary: "Keep tests beside the package and run them through musi test."
---

# Testing and Tooling

C++17 tests often call ordinary functions through a framework or `assert`:

```cpp
#include <cassert>

auto total(const int base_price, const int fee) -> int {
    return base_price + fee;
}

auto test_total() -> void {
    assert(total(1200, 45) == 1245);
}
```

Musi keeps checks beside the package with `@std/testing`.

{{snippet:cpp17-testing-tooling}}
