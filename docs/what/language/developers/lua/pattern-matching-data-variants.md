---
title: "Pattern Matching and Data Variants"
description: "Translate Lua tagged tables into Musi data variants and match patterns."
group: "Musi for Developers"
section: "Lua Developers"
order: 11
slug: "pattern-matching-data-variants"
summary: "Use variants when a value has one of several known shapes."
---

# Pattern Matching and Data Variants

Lua often represents alternatives with tagged tables:

```lua
local state = { tag = "running", id = 42 }

if state.tag == "running" then
  print(state.id)
end
```

Musi variants put the closed alternatives and payloads in one data definition.

{{snippet:lua-data-variants}}

`match` makes each case visible at the use site.
