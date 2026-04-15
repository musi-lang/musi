---
title: "Stdlib"
description: "Place the standard library on top of foundation and runtime so the layering stays clear."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 29
slug: "stdlib"
summary: "Reach for @std modules first in ordinary application code."
---

The standard library packages common language patterns behind stable imports. It gives everyday code names for options, results, slices, testing, environment access, FFI helpers, and more.

{{snippet:chapter-stdlib}}

Read `import "@std/option"` as a package import. Generic helpers such as `Option.unwrapOr[Int]` take type arguments when the operation needs to know the contained value type.

Pipeline-friendly helpers make data movement easier to read: start with a value, then apply transformations in order.

Continue to [Attributes](/learn/book/advanced/interop/attributes).
