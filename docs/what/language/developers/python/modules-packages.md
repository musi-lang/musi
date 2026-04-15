---
title: "Modules and Packages"
description: "Translate Python imports and package files into Musi exports, imports, and package paths."
group: "Musi for Developers"
section: "Python Developers"
order: 11
slug: "modules-packages"
summary: "Use export for public names and import for package or relative module boundaries."
---

Python modules export names by making them available at module scope:

```python
# ports.py
def default_port() -> int:
    return 8080
```

Musi marks public names with `export`.

{{snippet:python-module-export}}

Another file imports the module and calls the exported function:

```python
import ports

port = ports.default_port()
port
```

Musi imports the module value and reads exported names from it.

{{snippet:python-module-import}}

Package imports such as `@std/io` are for shared packages. Relative imports such as `./ports` are for nearby project files.
