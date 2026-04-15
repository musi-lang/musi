---
title: "Nil, Option, and Result"
description: "Translate Lua nil and multi-return failure conventions into Musi Option and Result values."
group: "Musi for Developers"
section: "Lua Developers"
order: 7
slug: "nil-option-result"
summary: "Use Option for absence and Result for failure that carries information."
---

# Nil, Option, and Result

Lua uses `nil` for absence, missing table fields, and many failure paths:

```lua
local ports = { admin = 9000 }
local port = ports.web
if port == nil then
  port = 8080
end
```

Musi uses the stdlib `Option` shape when a value may be absent.

{{snippet:lua-nil-option}}

Lua functions often return `nil` plus a message when failure carries information:

```lua
local function parsePort(text)
  if text == "8080" then
    return 8080
  end
  return nil, "invalid port"
end
```

Musi uses `Result` when callers should receive either a value or an error.

{{snippet:lua-result-value}}
