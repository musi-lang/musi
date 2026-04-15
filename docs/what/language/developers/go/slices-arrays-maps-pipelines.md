---
title: "Slices, Arrays, Maps, and Pipelines"
description: "Translate Go slices and map-style transformations into Musi collection pipelines."
group: "Musi for Developers"
section: "Go Developers"
order: 6
slug: "slices-arrays-maps-pipelines"
summary: "Use pipelines when collection work should read left to right."
---

# Slices, Arrays, Maps, and Pipelines

Go slices often grow with `append`:

```go
ports := []int{3000, 8080}
ports = append(ports, 9000)
visible := append([]int{}, ports...)
```

Musi pipelines keep the collection moving left to right.

{{snippet:go-slices-pipelines}}

Go maps are good for keyed lookup:

```go
ports := map[string]int{"web": 8080, "admin": 9000}
port := ports["web"]
```

When absence matters, Musi examples should return `Option` instead of relying on a zero value.

{{snippet:go-map-option}}
