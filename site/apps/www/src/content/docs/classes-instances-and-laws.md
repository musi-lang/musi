---
title: "Classes and instances"
description: "Read the class surface and define instances."
group: "Abstractions"
section: "Abstractions"
order: 11
slug: "classes-instances-and-laws"
summary: "Classes, methods, and instance declarations."
---

## What
Classes define shared behavior names.
Instances provide concrete implementations for those behavior names.

## Why
This pattern keeps behavior contracts explicit and avoids repeating equivalent helper sets.

## How
Define a class, then declare matching instances for concrete types.

{{snippet:class-eq}}

{{snippet:instance-eq-int}}

## When
Use classes when you want one operation (for example equality or formatting) to work across multiple domains.

## Analogy
Like interfaces in JavaScript/TypeScript, but with explicit instance attachment in the same language surface.

## Try it
Review both snippets, then continue to [Attributes and foreign declarations](/docs/attributes-and-foreign).
