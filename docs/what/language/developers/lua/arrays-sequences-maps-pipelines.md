---
title: "Arrays, Sequences, Maps, and Pipelines"
description: "Translate Lua sequence and table-map habits into Musi collection pipelines and Option values."
group: "Musi for Developers"
section: "Lua Developers"
order: 6
slug: "arrays-sequences-maps-pipelines"
summary: "Use pipelines for sequence work and Option when keyed lookup may be absent."
---

# Arrays, Sequences, Maps, and Pipelines

Lua sequences are tables with integer keys:

```lua
local ports = { 3000, 8080 }
table.insert(ports, 9000)
```

Musi pipelines keep the sequence moving left to right.

{{snippet:lua-arrays-pipelines}}

Lua tables also act as maps:

```lua
local ports = { web = 8080, admin = 9000 }
local port = ports.web
```

When absence matters, Musi examples return `Option` instead of relying on `nil`.

{{snippet:lua-map-option}}
