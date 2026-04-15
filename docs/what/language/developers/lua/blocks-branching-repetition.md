---
title: "Blocks, Branching, and Repetition"
description: "Translate Lua blocks, branching, and loops into Musi block expressions, match, recursion, and pipelines."
group: "Musi for Developers"
section: "Lua Developers"
order: 4
slug: "blocks-branching-repetition"
summary: "Use blocks as values and model repetition with recursion or library traversal."
---

# Blocks, Branching, and Repetition

Lua blocks group statements and return when a function should produce a value:

```lua
local function invoiceTotal()
  local basePrice = 1200
  local fee = 45
  return basePrice + fee
end
```

Musi blocks can produce the final value directly.

{{snippet:lua-block-expression}}

Lua loops commonly mutate loop state:

```lua
local function totalSeats(groups)
  local seats = 0
  for _ = 1, groups do
    seats = seats + 4
  end
  return seats
end
```

Musi uses recursion when the loop is a small state machine.

{{snippet:lua-recursive-control-flow}}
