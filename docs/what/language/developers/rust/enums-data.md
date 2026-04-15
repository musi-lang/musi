---
title: "Enums, Data, and Pattern Matching"
description: "Translate Rust enums with payloads into Musi data variants and matches."
group: "Musi for Developers"
section: "Rust Developers"
order: 5
slug: "enums-data"
summary: "Rust enum variants map to Musi `data` variants with named payloads and `match` arms."
---

Rust enum variants can carry named payloads:

```rust
enum Port {
    Configured { port: u16 },
    Default,
}

let selected = Port::Configured { port: 8080 };

match selected {
    Port::Configured { port } => port,
    Port::Default => 3000,
}
```

Musi puts payload labels inside constructor-style variant declarations.

{{snippet:rust-enum-data-match}}

Constructor calls use named payloads. Match arms read the value back through the variant shape.
