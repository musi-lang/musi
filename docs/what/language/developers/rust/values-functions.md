---
title: "Values, Functions, and Final Expressions"
description: "Translate Rust bindings, functions, and final expressions into Musi."
group: "Musi for Developers"
section: "Rust Developers"
order: 2
slug: "values-functions"
summary: "Rust `let`, `fn`, and final-expression habits map to Musi `let` functions and expression results."
---

Rust functions commonly end with an expression instead of an explicit `return`:

```rust
fn total(base: i32, fee: i32) -> i32 {
    base + fee
}

let answer = total(1200, 45);
answer
```

Musi uses the same expression-first idea, but function definitions are `let` bindings with parameters.

{{snippet:rust-values-functions}}

`total` is a name bound to a function. `answer` is a name bound to the call result. The last expression is the value of the block or file.

## Named arguments

Rust readers often use struct fields or builder methods when positional calls become unclear. Musi can name call arguments directly:

```rust
fn render(port: u16, secure: bool) -> u16 {
    port
}

let selected = render(8080, true);
selected
```

The Musi equivalent can keep positional order or spell labels at the call site.

{{snippet:rust-named-arguments}}

Use named arguments when two values have the same type or when swapped order would be easy to miss.
