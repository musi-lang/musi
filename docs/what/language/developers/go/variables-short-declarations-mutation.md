---
title: "Variables, Short Declarations, and Mutation"
description: "Translate Go `:=` and reassignment into explicit Musi bindings and mutation."
group: "Musi for Developers"
section: "Go Developers"
order: 4
slug: "variables-short-declarations-mutation"
summary: "Use mutable bindings for real changing state and fresh names for calculation stages."
---

# Variables, Short Declarations, and Mutation

Go uses `:=` for new locals and `=` for reassignment:

```go
visits := 0
visits = visits + 1

nextVisits := visits + 1
```

Musi marks the binding that can change.

{{snippet:go-variables-mutation}}

## Fresh values

Go often uses new names for calculation stages:

```go
basePrice := 1200
total := basePrice + 45
```

Musi bindings are stable unless marked mutable, so fresh names fit ordinary calculations.

{{snippet:go-fresh-value}}
