---
title: "Headers, Modules, and Packages"
description: "Translate C99 headers and translation units into Musi exports and package imports."
group: "Musi for Developers"
section: "C Developers"
order: 12
slug: "headers-modules-packages"
summary: "Expose package-facing values with export and import dependencies by path."
---

# Headers, Modules, and Packages

C99 exposes package-facing declarations through headers:

```c
#ifndef PORTS_H
#define PORTS_H

int ports_default(void);

#endif
```

Musi exports the package-facing value directly.

{{snippet:c99-module-export}}

C source files include headers to use another translation unit:

```c
#include "ports.h"

int selected_port = ports_default();
```

Musi imports package values directly.

{{snippet:c99-module-import}}
