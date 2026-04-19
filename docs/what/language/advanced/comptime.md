---
title: "Comptime"
description: "Run selected Musi expressions during compilation and use the result as a first-class value."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 33
slug: "comptime"
summary: "Use value-position comptime for constants, specialization, and generated syntax."
---
Compile-time code runs while the program is being prepared, not while the user is running it. It is useful for generating repetitive structure, checking facts early, or specializing code from known information.

{{snippet:chapter-comptime}}

Use compile-time work when it removes real repetition or catches real mistakes. Do not use it to hide simple code behind machinery.

## The bakery rule

If runtime is serving customers, compile time is preparing trays before the doors open. It can label pastries and arrange shelves, but it should not depend on which customer walks in next.

## Keep generated shapes readable

Generated declarations should still look like normal declarations when inspected. If a reader cannot understand what compile-time code produced, diagnostics and reviews become harder.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
