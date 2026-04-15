---
title: "Coroutines and Effect Boundaries"
description: "Translate Lua coroutine yield and resume habits into Musi effect requests and handlers."
group: "Musi for Developers"
section: "Lua Developers"
order: 9
slug: "coroutines-effect-boundaries"
summary: "Use effects to name operations and handlers to decide how suspended work continues."
---

# Coroutines and Effect Boundaries

Lua coroutines can pause and resume a computation:

```lua
local worker = coroutine.create(function()
  coroutine.yield(8080)
  return 9000
end)

local ok, port = coroutine.resume(worker)
```

Musi names the operation as an effect request, then handles it at a boundary.

{{snippet:lua-effect-request}}

Handlers make the answer to an operation visible where the computation is interpreted.

{{snippet:lua-handler-boundary}}
