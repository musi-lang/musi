---
title: "Overview"
description: "Translate Lua 5.4.8 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "Lua Developers"
order: 1
slug: "overview"
summary: "Start from Lua 5.4.8 habits, then read equivalent Musi expression, data, effect, and package shapes."
---

# Musi for Lua Developers

Lua 5.4.8 is the comparison point for this guide. Lua developers often bring dynamic values, tables, metatables, closures, modules, `nil`, `pcall`, coroutines, embedding, and C API boundaries. Musi keeps those jobs recognizable while making values, data shape, and outside work explicit.

Lua usually starts with a small function and a local result:

```lua
local function total(basePrice, fee)
  return basePrice + fee
end

local answer = total(1200, 45)
```

Musi gives the function explicit input and output types. The body expression produces the result.

{{snippet:lua-values-locals-expressions}}

## Reading path

Read the guide from everyday Lua toward deeper translation points:

1. locals, functions, closures, blocks, and repetition;
2. tables, sequences, maps, nil, results, and effects;
3. coroutines, metatables, variants, modules, tests, and native boundaries.
