---
title: "Quote and Syntax"
description: "Introduce quote and syntax work late so beginners are not overloaded too early."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "quote-and-syntax"
summary: "Treat code as data only after ordinary code reading feels natural."
---
Quote and syntax features let code talk about code as data. They are useful for tools, templates, generated declarations, and compile-time transformations.

{{snippet:chapter-quote-and-syntax}}

This is powerful because the program can inspect or build structure that normally belongs to source text. It is also easy to overuse.

## Use syntax values for tooling-shaped work

Code generation, lint-like checks, and declaration templates are good fits. A normal receipt calculation or animal-name formatter should stay ordinary code.

## Generated code should still feel authored

If a template produces declarations, those declarations should be names a person would have written. Tooling should help the reader, not bury intent behind syntax tricks.

A good test for quoted syntax is whether the generated shape could be explained to a teammate without mentioning the generator first. If the useful result is “a handler for each payment event,” the generated declarations should show those handlers plainly. The quote is the workshop; the resulting code is what people maintain.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
