---
title: "Overview"
description: "Translate Rust 1.87.0 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "Rust Developers"
order: 1
slug: "overview"
summary: "Start from Rust habits, then read the equivalent Musi expression, data, abstraction, effect, and FFI shapes."
---

Rust 1.87.0 is the comparison point for this guide. Each page starts with Rust code, then shows the Musi equivalent with the same names and the same job.

A Rust reader should not have to skim a generic Musi example and guess the translation. The Musi block below each Rust block is the translation.

## First translation

Rust often starts with a small function and a binding for the result:

```rust
fn total(base: i32, fee: i32) -> i32 {
    base + fee
}

let answer = total(1200, 45);
answer
```

Musi writes the same calculation as a `let` function. The final expression leaves the value.

{{snippet:rust-values-functions}}

## Reading path

Read these pages in order if you are moving a Rust habit into Musi code:

- [Values, Functions, and Final Expressions](/learn/book/developers/guides/rust/values-functions)
- [Mutation](/learn/book/developers/guides/rust/mutation)
- [Records, Structs, and Field Updates](/learn/book/developers/guides/rust/records-structs)
- [Enums, Data, and Pattern Matching](/learn/book/developers/guides/rust/enums-data)
- [Traits, Classes, Instances, and Laws](/learn/book/developers/guides/rust/traits-classes-laws)
- [Generics and Type Constructors](/learn/book/developers/guides/rust/generics)
- [Results, Requests, and Effects](/learn/book/developers/guides/rust/results-effects)
- [Modules, Packages, and Visibility](/learn/book/developers/guides/rust/modules-packages)
- [Unsafe and FFI](/learn/book/developers/guides/rust/unsafe-ffi)
- [Testing and Tooling](/learn/book/developers/guides/rust/testing-tooling)

## Rust habits that transfer

- name data by domain
- keep functions small
- make boundary work visible
- use types to explain what values can be
- test behavior close to the code that owns it

## Rust habits to translate

- `let mut name` becomes a binding to a mutable value
- `return` is not used for ordinary final values
- `enum` variants become `data` variants
- `trait` and `impl` become `class`, `instance`, and laws when rules matter
- `Result` handles recoverable data failure; effects handle requested outside work
- raw native work stays behind `foreign` and `unsafe`
