---
title: "Values, Locals, and Expressions"
description: "Translate Lua locals and expression habits into Musi typed bindings and expression-bodied functions."
group: "Musi for Developers"
section: "Lua Developers"
order: 2
slug: "values-locals-expressions"
summary: "Use typed function signatures and stable bindings for values that should stay clear."
---

# Values, Locals, and Expressions

Lua locals are dynamic and can hold any runtime value:

```lua
local function total(basePrice, fee)
  return basePrice + fee
end

local answer = total(1200, 45)
```

Musi keeps names local, but it records the input and output types at the function boundary.

{{snippet:lua-values-locals-expressions}}

Lua code often relies on names and comments to explain argument meaning:

```lua
local selected = render(8080, true)
```

Musi supports named calls when labels make the call site clearer.

{{snippet:lua-named-calls}}
