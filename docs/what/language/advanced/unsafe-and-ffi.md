---
title: "Unsafe and FFI"
description: "Use unsafe blocks and @std/ffi when Musi code crosses raw native boundaries."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "unsafe-and-ffi"
summary: "Keep raw pointer and native-call work behind a visible unsafe boundary."
---
Unsafe and FFI code is where Musi talks to places the compiler cannot fully protect. It is necessary for some systems work, but it should be narrow and explicit.

{{snippet:chapter-unsafe-and-ffi}}

The best unsafe boundary is small, named, and wrapped by safe code. Most of the package should not know the boundary exists.

## Put danger behind a counter

Like a workshop tool with a guard, unsafe code belongs in a controlled area. Check inputs before crossing the boundary and return typed results after crossing back.

## Document the contract in code shape

Use precise names and types so the contract is visible. If a pointer must be non-null or a buffer length must match, make the wrapper express that requirement.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
