---
title: "Stdlib"
description: "Place the standard library on top of foundation and runtime so the layering stays clear."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 29
slug: "stdlib"
summary: "Reach for @std modules first in ordinary application code."
---

The standard library packages common language patterns behind flat ES6-style imports. It gives everyday code names for options, results, lists, paths, bytes, testing, environment access, OS facts, low-level target facts, FFI helpers, and more.

{{snippet:chapter-stdlib}}

Read `import "@std/option"` as a module import. Use lower-case module aliases such as `option`, `result`, `path`, and `fs`; reserve PascalCase for public types such as `Option`, `Result`, `Path`, and `Bytes`.

Prefer domain types at API boundaries. `@std/path.Path`, `@std/bytes.Bytes`, `@std/env.Key`, `@std/process.ExitCode`, and `@std/time.Duration` avoid raw strings or integers where the meaning matters.

Use `@std/os` for typed platform checks in ordinary code. Use `@std/sys` only when a low-level wrapper needs raw target facts such as `sys.os`, `sys.arch`, `sys.family`, or `sys.jit`. Those facts reflect the Rust-supported platform universe and the future Cranelift JIT subset.

Many helpers keep the subject as the first argument so module calls and receiver calls can share one reading order when the compiler supports the receiver form.

Continue to [Attributes](/learn/book/advanced/interop/attributes).
