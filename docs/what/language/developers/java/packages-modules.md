---
title: "Packages and Modules"
description: "Translate Java packages and modules into Musi imports, exports, and package boundaries."
group: "Musi for Developers"
section: "Java Developers"
order: 12
slug: "packages-modules"
summary: "Use import and export directly to describe package-facing boundaries."
---

# Packages and Modules

Java package-facing code often starts with a package declaration and public type:

```java
package ports;

public final class Ports {
    public static int defaultPort() {
        return 8080;
    }
}
```

Musi exports the package-facing value directly.

{{snippet:java-module-export}}

Java imports another package member by name:

```java
import ports.Ports;

int port = Ports.defaultPort();
```

Musi imports a module value and calls through it.

{{snippet:java-module-import}}

