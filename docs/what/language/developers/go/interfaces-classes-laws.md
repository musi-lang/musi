---
title: "Interfaces, Classes, Instances, and Laws"
description: "Translate Go interfaces into Musi classes, instances, and laws."
group: "Musi for Developers"
section: "Go Developers"
order: 10
slug: "interfaces-classes-laws"
summary: "Use classes for required behavior and laws for behavior expectations."
---

# Interfaces, Classes, Instances, and Laws

Go interfaces name behavior structurally:

```go
type Vehicle interface {
    Wheels() int
}

type Car struct{}

func (Car) Wheels() int {
    return 4
}
```

Musi classes state required operations. Instances attach those operations to a type.

{{snippet:go-interface-class-law}}

A real-world analogy: a car is a vehicle, but road rules require at least four wheels for this category. The class names the operation. The law records the expectation implementations should satisfy.
