---
title: "Attributes"
description: "Use attributes to attach metadata for compiler-known items, layout, foreign links, diagnostics, and lifecycle information."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 30
slug: "attributes"
summary: "Attributes describe metadata, boundaries, and build-time intent without changing Musi into a macro language."
---
Attributes attach extra information to declarations. They are useful when tools, generated code, foreign boundaries, or known built-ins need metadata that is not part of the ordinary value expression.

{{snippet:chapter-attributes}}

An attribute should be treated like a label on a file folder. It helps a tool handle the declaration correctly, but it should not become a secret second language for normal program logic.

## Metadata with a job

Use attributes for facts a compiler, formatter, runtime bridge, or documentation tool must know. If the fact is ordinary domain data, put it in a value instead.

## Keep labels rare

Too many attributes make code feel like a form full of stamps. Prefer normal declarations unless the extra metadata changes how tooling must treat the declaration.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
