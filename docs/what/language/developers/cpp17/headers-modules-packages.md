---
title: "Headers, Modules, and Packages"
description: "Translate C++17 headers, namespaces, and source organization into Musi exports and package imports."
group: "Musi for Developers"
section: "C++ Developers"
order: 12
slug: "headers-modules-packages"
summary: "Expose package-facing values with export and import dependencies by path."
---

# Headers, Namespaces, and Packages

C++17 exposes package-facing declarations through headers:

```cpp
#pragma once

namespace ports {
auto default_port() -> int;
}
```

Musi exports the package-facing value directly.

{{snippet:cpp17-module-export}}

C++ source files include headers and call through namespaces:

```cpp
#include "ports.hpp"

auto selected_port() -> int {
    return ports::default_port();
}
```

Musi imports package values directly.

{{snippet:cpp17-module-import}}
