---
title: "Generics and Type Constructors"
description: "Translate Go generic functions and generic data into Musi type parameters and type constructors."
group: "Musi for Developers"
section: "Go Developers"
order: 11
slug: "generics-type-constructors"
summary: "Use bracketed type parameters for reusable functions, data, and higher-kinded constraints."
---

# Generics and Type Constructors

Go generics put type parameters after the function name:

```go
func Identity[T any](input T) T {
    return input
}

port := Identity[int](8080)
```

Musi also uses bracketed type parameters for reusable functions.

{{snippet:go-generic-function}}

Generic data follows the same pattern.

```go
type Box[T any] struct {
    Value T
}

box := Box[int]{Value: 8080}
```

Musi can pass a generic data constructor where a class expects a type constructor.

{{snippet:go-generic-data}}
