---
title: "Foundation"
description: "Separate language foundation from runtime and stdlib layers."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 27
slug: "foundation"
summary: "Understand what belongs to musi:core before reaching for stdlib modules."
---

Foundation modules provide compiler-known language roots. They are the bridge between surface syntax and names the compiler or runtime must recognize.

{{snippet:chapter-foundation}}

Read `import "musi:core"` as importing a reserved foundation module. It is not a normal package dependency; it names part of the language foundation.

Most user code should start with ordinary package imports. Reach for foundation modules only when the docs or standard library say a language-level root is needed.

Continue to [Runtime](/learn/book/effects-runtime/runtime-model/runtime).
