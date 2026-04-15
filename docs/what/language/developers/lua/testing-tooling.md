---
title: "Testing and Tooling"
description: "Translate Lua test habits into small Musi package tests and toolchain checks."
group: "Musi for Developers"
section: "Lua Developers"
order: 13
slug: "testing-tooling"
summary: "Write tests as small examples and use the package toolchain for routine checks."
---

# Testing and Tooling

Lua tests often call a function and raise an error when the expectation fails:

```lua
local function testDefaultPort()
  if defaultPort() ~= 8080 then
    error("default port mismatch")
  end
end
```

Musi tests keep the rule name, the small value, and the expectation together.

{{snippet:lua-testing-tooling}}

Use the package toolchain for formatting, checking, and running tests so local work matches CI behavior.
