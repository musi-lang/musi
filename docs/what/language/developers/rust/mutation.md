---
title: "Mutation"
description: "Translate Rust 2024 edition (1.85+) mutation habits into Musi's value-based mutation model."
group: "Musi for Developers"
section: "Rust Developers"
order: 3
slug: "mutation"
summary: "Read Musi mutation as a mutable value binding, not as a variable-mode prefix."
---

Rust marks the binding mutable:

```rust
let mut visits = 0;
visits += 1;
visits
```

Musi keeps mutability on the value owned by the binding.

{{snippet:rust-mutation-counter}}

Read `let visits := mut 0;` as a normal name bound to a value that may change. The update writes the next value back into that mutable value.

## When to translate `let mut`

Use Musi mutation for Rust code where the same local state changes over time:

- counters
- parser cursors
- reusable buffers
- local totals built over several steps

If Rust uses `mut` only to stage a clearer next value, Musi often reads better with a new `let` name.

```rust
let base = 1200;
let total = base + 45;
total
```

{{snippet:rust-fresh-value}}
