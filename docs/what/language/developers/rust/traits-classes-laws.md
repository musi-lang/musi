---
title: "Traits, Classes, Instances, and Laws"
description: "Translate Rust traits and impls into Musi classes, instances, and laws."
group: "Musi for Developers"
section: "Rust Developers"
order: 6
slug: "traits-classes-laws"
summary: "Rust trait contracts map to Musi classes, instances, and explicit laws when behavior has rules."
---

Rust traits name shared behavior. Implementations attach that behavior to a type:

```rust
trait Vehicle {
    fn wheels(&self) -> usize;

    fn at_least_four_wheels(&self) -> bool {
        self.wheels() >= 4
    }
}

struct Car;

impl Vehicle for Car {
    fn wheels(&self) -> usize {
        4
    }
}
```

Musi separates the shape from the instance and writes the rule as a law.

{{snippet:rust-trait-class-law}}

A car is a vehicle, but the law says which vehicle instances count as valid in this model. The class names behavior. The instance provides behavior. The law names the expectation readers can rely on.
