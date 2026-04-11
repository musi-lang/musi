---
title: "Records and arrays"
description: "Use record literals, arrays, and explicit spread forms."
group: "Core language"
section: "Core language"
order: 8
slug: "records-arrays-and-mutation"
summary: "Structured values and the current writeable-data surface."
---

## What
Records and arrays are ordinary values with predictable update patterns.

{{snippet:record-array}}

## Why
They keep data structured and avoid mixing unrelated values into one flat tuple.

## How
Build values with literals, then use spread/update forms when you need a modified copy.

{{snippet:spread-record-array}}

## When
Use these forms for request payloads, config objects, and small in-memory collections.

## Analogy
Like object/array literals in JS, with explicit update syntax.

## Try it
Create a base value then build one spread-based variant, then continue to [Effects and handlers](/docs/effects-and-handlers).
