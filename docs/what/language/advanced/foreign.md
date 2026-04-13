---
title: "Foreign"
description: "Keep foreign declarations separate from general attributes so boundary thinking stays clear."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 31
slug: "foreign"
summary: "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
---

{{snippet:foreign-puts}}

## What

Foreign declarations bind Musi code to external symbols and ABIs.

## Why

Cross-language boundaries are powerful but sharp, so they belong at edges.

## How

- Declare the ABI explicitly.
- Keep the surface narrow.
- Wrap foreign calls behind cleaner helpers when possible.

## Try it

- Read one foreign declaration.
- List the boundary facts it exposes.
- Sketch the wrapper you would place around it.

## Common mistake

Do not scatter foreign declarations through ordinary business code.

## Next

Continue to [Quote and syntax](/docs/language/advanced/quote-and-syntax).
