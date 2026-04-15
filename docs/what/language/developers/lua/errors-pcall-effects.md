---
title: "Errors, Pcall, and Effects"
description: "Translate Lua error and pcall habits into Musi Result values and effect boundaries."
group: "Musi for Developers"
section: "Lua Developers"
order: 8
slug: "errors-pcall-effects"
summary: "Keep expected failure as data and outside work behind effectful APIs."
---

# Errors, Pcall, and Effects

Lua can raise errors and catch them with `pcall`:

```lua
local function parsePort(text)
  if text == "8080" then
    return 8080
  end
  error("parse error")
end

local ok, port = pcall(parsePort, "abc")
if not ok then
  port = 3000
end
```

Musi keeps expected failure in the returned value.

{{snippet:lua-errors-results}}

Lua host interaction often calls global or module functions directly:

```lua
io.write("name> ")
local name = io.read("*l")
print(name)
```

Musi keeps console work at effectful boundaries.

{{snippet:lua-effect-boundary}}
