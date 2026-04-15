---
title: "Headers, Modules, and Packages"
description: "Translate headers, source files, and C++ modules into Musi imports, exports, and package boundaries."
group: "Musi for Developers"
section: "C/C++ Developers"
order: 12
slug: "headers-modules-packages"
summary: "Use import and export directly to describe package-facing boundaries."
---

# Headers, Modules, and Packages

C separates declarations into headers and definitions into source files. C++23 modules reduce some header friction, but projects still need careful boundaries. Musi packages use imports and exports directly.

## Export a package-facing value

C exposes a declaration from a header and puts the definition in a source file:

```c
// ports.h
int default_port(void);

// ports.c
int default_port(void) {
    return 8080;
}
```

C++23 modules can make the export direct:

```cpp
export module ports;

export auto default_port() -> int {
    return 8080;
}
```

Musi exports the package-facing value from the file that defines it.

{{snippet:c-cpp-module-export}}

`export` marks the value as part of the package surface.

## Import from another file

C imports declarations with a preprocessor include:

```c
#include "ports.h"

int port = default_port();
```

C++23 imports a named module:

```cpp
import ports;

auto port = default_port();
```

Musi imports the module value and calls through it.

{{snippet:c-cpp-module-import}}

Prefer small modules with clear names. If a file starts mixing parsing, checking, IO, and formatting, split it by responsibility.
