---
title: "Runtime"
description: "Learn what runtime-backed imports are for and why they are separate from stdlib helpers."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 28
slug: "runtime"
summary: "Use musi:runtime for runtime-backed capabilities and host services."
---

Runtime modules provide host-backed services such as environment, process, filesystem, time, random, logging, and dynamic module loading.

{{snippet:chapter-runtime}}

Read `import "musi:runtime"` as opening the runtime service surface. Runtime operations may depend on the host process, filesystem, clock, or environment.

Prefer standard-library wrappers when they exist. They give runtime operations stable names and keep host details out of ordinary modules.

Continue to [Stdlib](/learn/book/effects-runtime/runtime-model/stdlib).
