---
title: "Generics and Type Constructors"
description: "Translate Rust generic functions and generic data into Musi type parameters."
group: "Musi for Developers"
section: "Rust Developers"
order: 7
slug: "generics"
summary: "Rust generic functions and structs map to Musi bracketed type parameters and constructor-style data."
---

Rust generic functions name the type parameter before the function parameters:

```rust
fn identity<T>(input: T) -> T {
    input
}

let port = identity::<u16>(8080);
port
```

Musi uses bracketed type parameters on the `let` function.

{{snippet:rust-generic-function}}

Generic data follows the same bracketed shape:

```rust
struct Box1<T> {
    value: T,
}

let boxed = Box1 { value: 8080 };
boxed.value
```

{{snippet:rust-generic-data}}

The Musi match reads the payload back out through the constructor shape.

When a class receives a type constructor, Musi can name that constructor in the type parameter list.

{{snippet:rust-type-constructor-class}}
