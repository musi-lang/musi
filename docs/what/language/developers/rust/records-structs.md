---
title: "Records, Structs, and Field Updates"
description: "Translate Rust structs and update syntax into Musi records."
group: "Musi for Developers"
section: "Rust Developers"
order: 4
slug: "records-structs"
summary: "Rust structs map to named Musi record shapes and spread updates."
---

Rust structs group named fields:

```rust
struct Endpoint {
    host: String,
    port: u16,
    secure: bool,
}

let local = Endpoint {
    host: String::from("localhost"),
    port: 8080,
    secure: false,
};

let secure = Endpoint {
    secure: true,
    ..local
};

secure.port
```

Musi uses record-shaped data for the field contract and record literals for values.

{{snippet:rust-struct-record}}

The spread update starts from `local`, then replaces only `secure`. Field access stays direct: `secure.port`.
