---
title: "Instances"
description: "Learn instance declarations only after classes make sense on their own."
group: "Abstractions"
section: "Abstractions"
order: 22
slug: "instances"
summary: "Attach concrete behavior to concrete types."
---

{{snippet:instance-eq-int}}

## What

An instance says how one specific type satisfies a class.

## Why

Splitting classes from instances reduces mental overload.

## How

- Choose one class.
- Choose one concrete type.
- Implement the required members in one instance block.

## Try it

- Take a tiny class.
- Add one instance for `Int`.
- Use it once.

## Common mistake

Do not define several classes and instances at once during early practice.

## Next

Continue to [Laws](/docs/language/abstractions/laws).
