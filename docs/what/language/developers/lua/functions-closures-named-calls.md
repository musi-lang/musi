---
title: "Functions, Closures, and Named Calls"
description: "Translate Lua function values and closures into Musi functions, lambdas, and named calls."
group: "Musi for Developers"
section: "Lua Developers"
order: 3
slug: "functions-closures-named-calls"
summary: "Use functions for named behavior and lambdas for small callback-shaped values."
---

# Functions, Closures, and Named Calls

Lua treats functions as ordinary values:

```lua
local function makeAdder(step)
  return function(value)
    return value + step
  end
end

local addOne = makeAdder(1)
local nextPort = addOne(8080)
```

Musi lambdas use a leading backslash, so callback-shaped values stand out at the call site.

{{snippet:lua-function-closure}}

Use named calls when the meaning of a Lua positional argument would otherwise depend on memory.

{{snippet:lua-named-calls}}
