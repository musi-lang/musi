---
title: "Attributes"
description: "Introduce attributes separately from foreign declarations so each concept stays narrow."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 30
slug: "attributes"
summary: "Use attributes when the compiler or runtime needs explicit extra metadata."
---

{{snippet:chapter-attributes}}

## What

Attributes attach explicit metadata to declarations.
This example keeps that concrete by placing `@link(...)` on a foreign declaration, where the extra metadata clearly changes how surrounding system should treat the declaration.
Attributes are not everyday syntax, but they are important when code must talk to tooling, compiler, or runtime machinery.

## Why

Users eventually ask how to express non-local facts such as linkage, platform detail, or compiler-facing metadata.
Those questions should not clutter beginner chapters, but they still need a clean answer.
An attribute chapter gives those answers without pretending attributes are part of normal domain modeling.

## How

Read the attribute as metadata attached to declaration that follows it.
Then read the foreign declaration itself and separate two concerns: the declaration says what binding exists, the attribute says extra information needed for that binding to work correctly in broader system.
When using attributes, keep them narrow, explicit, and close to declarations that truly require them.

## Try it

- Add one attribute to a boundary-facing declaration.
- State what behavior the attribute is trying to influence.
- Remove it mentally and decide what information would then be missing.

## Common mistake

Do not use attributes to hide core business logic that should be visible in ordinary code structure.

## Next

Continue to [Foreign](/docs/language/advanced/foreign) to focus on declarations that cross out of Musi entirely.
