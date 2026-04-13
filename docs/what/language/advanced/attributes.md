---
title: "Attributes"
description: "Introduce attributes separately from foreign declarations so each concept stays narrow."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 30
slug: "attributes"
summary: "Use attributes when the compiler or runtime needs explicit extra metadata."
---

{{snippet:attr-link-foreign}}

## What

Attributes attach structured metadata to declarations.

## Why

Readers should see attributes as explicit metadata, not mysterious decoration.

## How

- Put the attribute on the declaration it modifies.
- Keep first uses small.
- Use attributes only when plain syntax cannot say the same thing clearly.

## Try it

- Read one attributed declaration.
- Identify what the attribute changes.
- Decide whether plain syntax could have expressed it.

## Common mistake

Do not collect attributes casually.

## Next

Continue to [Foreign](/docs/language/advanced/foreign).
