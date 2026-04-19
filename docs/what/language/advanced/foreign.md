---
title: "Foreign"
description: "Keep foreign declarations separate from general attributes so boundary thinking stays clear."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 31
slug: "foreign"
summary: "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
---
Foreign declarations describe things that live outside normal Musi source. They are useful when code must call into a host library, runtime symbol, or external system boundary.

{{snippet:chapter-foreign}}

Treat foreign boundaries with care. The compiler has less information there, so names, types, and attributes need to be especially honest.

## Adapters protect the rest of the program

Put foreign calls behind small Musi wrappers. A weather app might call a native API at the edge, then return a normal record to the rest of the package.

## Keep uncertainty contained

If the foreign side can fail, return a shape that says so. Do not let low-level uncertainty leak through every business function.

Foreign names also deserve extra care because editor tooling cannot infer intent from the other language. A short wrapper can turn `native_read_file` into a Musi function that returns text or a typed failure. After that point, the rest of the program reads like ordinary Musi again.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
