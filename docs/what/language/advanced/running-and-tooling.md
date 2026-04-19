---
title: "Running and Tooling"
description: "Bring the learning path back to commands and workflow once language basics are in place."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 34
slug: "running-and-tooling"
summary: "Finish with the everyday command flow for checking, running, and building code."
---
Running and tooling are how source code becomes daily work. The formatter, language server, tests, build commands, and package metadata should agree with each other.

{{snippet:chapter-running-and-tooling}}

Good tooling behavior is predictable. Formatting a file in the editor should match formatting it from the command line. Diagnostics should point at the same source the compiler understands.

## Editor and command line should match

A teammate using VS Code and a teammate using a terminal should not get different source shapes. Put shared behavior in project configuration rather than personal editor settings.

## Tight loops win

Use the smallest command that answers the current question. Run a file while learning, run tests when changing behavior, run the full build before shipping.

Project tooling should also be boring in review. If formatting, generated docs, or package checks require a special local ritual, put that ritual in a script or Makefile target. The less a contributor has to remember, the more likely the project stays consistent.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
