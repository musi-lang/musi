---
title: "Tables, Records, and Field Updates"
description: "Translate Lua table records into Musi records and field updates."
group: "Musi for Developers"
section: "Lua Developers"
order: 5
slug: "tables-records-field-updates"
summary: "Use records when table fields are known parts of the data shape."
---

# Tables, Records, and Field Updates

Lua tables often act as records:

```lua
local localEndpoint = { host = "localhost", port = 8080, secure = false }
local publicEndpoint = {
  host = "api.example.com",
  port = localEndpoint.port,
  secure = true,
}
```

Musi record-style data keeps the field names visible and typed.

{{snippet:lua-tables-records}}

Use field updates when the new value is mostly the old value with a few changed fields.
