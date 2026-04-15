---
title: "Namespaces, Modules, and Packages"
description: "Translate C# namespaces, files, and NuGet-style package habits into Musi exports, imports, and package paths."
group: "Musi for Developers"
section: "C# Developers"
order: 12
slug: "namespaces-modules-packages"
summary: "Use export for public names and import for package or relative module boundaries."
---

C# namespaces and projects organize public APIs:

```csharp
namespace Ports;

public static class Defaults
{
    public static int DefaultPort() => 8080;
}
```

Musi marks public names with `export`.

{{snippet:csharp-module-export}}

Another file imports the module and calls the exported function.

```csharp
using Ports;

var port = Defaults.DefaultPort();
port;
```

Musi imports the module value and reads exported names from it.

{{snippet:csharp-module-import}}

Package imports such as `@std/io` are for shared packages. Relative imports such as `./ports` are for nearby project files.
