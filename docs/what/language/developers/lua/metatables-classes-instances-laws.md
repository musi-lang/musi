---
title: "Metatables, Classes, Instances, and Laws"
description: "Translate Lua metatable behavior into Musi classes, instances, and laws."
group: "Musi for Developers"
section: "Lua Developers"
order: 10
slug: "metatables-classes-instances-laws"
summary: "Use classes for required behavior and laws for behavior expectations."
---

# Metatables, Classes, Instances, and Laws

Lua metatables let tables share behavior:

```lua
local Vehicle = {}
Vehicle.__index = Vehicle

function Vehicle:wheels()
  return 4
end

local car = setmetatable({}, Vehicle)
```

Musi classes state required operations. Instances attach those operations to a type.

{{snippet:lua-metatable-class-law}}

A real-world analogy: a car is a vehicle, but road rules require at least four wheels for this category. The class names the operation. The law records the expectation implementations should satisfy.
