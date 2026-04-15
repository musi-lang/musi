---
title: "Packages, Imports, and Exports"
description: "Translate Go packages and exported names into Musi imports and exports."
group: "Musi for Developers"
section: "Go Developers"
order: 13
slug: "packages-imports-exports"
summary: "Use import and export directly to describe package-facing boundaries."
---

# Packages, Imports, and Exports

Go exposes package members by capitalizing exported names:

```go
package ports

func DefaultPort() int {
    return 8080
}
```

Musi exports the package-facing value directly.

{{snippet:go-module-export}}

Go imports a package and calls through its package name:

```go
import "example.com/app/ports"

port := ports.DefaultPort()
```

Musi imports a module value and calls through it.

{{snippet:go-module-import}}
