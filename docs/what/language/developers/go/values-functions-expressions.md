---
title: "Values, Functions, and Expressions"
description: "Translate Go functions and expression habits into Musi expression-bodied functions and named calls."
group: "Musi for Developers"
section: "Go Developers"
order: 2
slug: "values-functions-expressions"
summary: "Use expression bodies, positional calls, and named calls for readable functions."
---

# Values, Functions, and Expressions

Go functions name parameters and return types, then use `return` to produce the result:

```go
func total(basePrice int, fee int) int {
    return basePrice + fee
}

answer := total(1200, 45)
```

Musi keeps the signature explicit. The function body is the result expression.

{{snippet:go-values-functions}}

## Named calls

Go call sites are positional:

```go
selected := render(8080, true)
```

That works, but booleans and repeated numbers become harder to read. Musi lets the call site name arguments when labels carry meaning.

{{snippet:go-named-calls}}
