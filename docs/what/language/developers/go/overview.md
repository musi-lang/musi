---
title: "Overview"
description: "Translate Go 1.26.2 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "Go Developers"
order: 1
slug: "overview"
summary: "Start from Go 1.26.2 habits, then read equivalent Musi expression, data, effect, and package shapes."
---

# Musi for Go Developers

Go 1.26.2 is the comparison point for this guide. Go developers often bring short declarations, explicit errors, slices, maps, structs, interfaces, goroutines, channels, context cancellation, packages, tests, `unsafe`, and cgo boundaries. Musi keeps those jobs recognizable while making values, data shape, and outside work explicit.

Go usually starts with a small function and a call:

```go
func total(basePrice int, fee int) int {
    return basePrice + fee
}

answer := total(1200, 45)
```

Musi gives the function the same inputs and result, then lets the body expression produce the value.

{{snippet:go-values-functions}}

## Reading path

Read the guide from everyday Go toward deeper translation points:

1. values, functions, blocks, and mutation;
2. structs, slices, maps, nil, results, and effects;
3. goroutines, channels, context, interfaces, generics, and receiver calls;
4. packages, tests, unsafe, cgo, and FFI boundaries.
