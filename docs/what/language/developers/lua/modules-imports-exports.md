---
title: "Modules, Imports, and Exports"
description: "Translate Lua require-return modules into Musi imports and exports."
group: "Musi for Developers"
section: "Lua Developers"
order: 12
slug: "modules-imports-exports"
summary: "Use import and export directly to describe package-facing boundaries."
---

# Modules, Imports, and Exports

Lua modules commonly return a table:

```lua
local Ports = {}

function Ports.defaultPort()
  return 8080
end

return Ports
```

Musi exports the package-facing value directly.

{{snippet:lua-module-export}}

Lua imports another module with `require`:

```lua
local Ports = require("ports")
local port = Ports.defaultPort()
```

Musi imports a module value and calls through it.

{{snippet:lua-module-import}}
