---
title: "Testing and Tooling"
description: "Translate Rust unit-test habits into Musi package tests."
group: "Musi for Developers"
section: "Rust Developers"
order: 11
slug: "testing-tooling"
summary: "Rust `#[test]` habits map to Musi package tests with `@std/testing`."
---

Rust usually keeps a small test beside the code that owns the behavior:

```rust
fn default_port() -> u16 {
    8080
}

#[test]
fn default_port_is_http_alt() {
    assert_eq!(default_port(), 8080);
}
```

Musi writes a test function and uses `@std/testing` assertions.

{{snippet:rust-testing-tooling}}

A package test should name behavior, call the function under test, and return the test runner's result expression.
