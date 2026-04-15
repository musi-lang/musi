---
title: "Modules, Packages, and Visibility"
description: "Translate Rust module exports and package calls into Musi imports and exports."
group: "Musi for Developers"
section: "Rust Developers"
order: 9
slug: "modules-packages"
summary: "Rust `pub mod` habits map to Musi exported values and imported packages."
---

Rust module code often exposes a small public function:

```rust
pub mod ports {
    pub fn default_port() -> u16 {
        8080
    }
}

let port = ports::default_port();
port
```

Musi exports names from a file or package and imports that package where the names are needed.

{{snippet:rust-module-export}}

A consumer reads the exported name through the imported package value.

{{snippet:rust-module-import}}

Keep exports small. Export names a reader should depend on; leave local helpers unexported.
