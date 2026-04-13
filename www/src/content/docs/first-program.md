---
title: "First program"
description: "Write a small file and read it as expressions."
group: "Start"
section: "Start"
order: 2
slug: "first-program"
summary: "A first Musi file without extra ceremony."
---

Musi files are read as expressions.
Read top to bottom. Each expression leaves a result for the next one to use.

## Smallest runnable file

{{snippet:first-file}}

`let` binds a name. `;` ends an expression. That is enough to start writing and running Musi code.

## Add one function

{{example:double-function}}

Functions use the same `let` form as values. You do not switch to a separate declaration syntax.

## Try it

{{try:first-program}}

## Next step

Type the snippets above, run them with `music`, then move to [Files, packages, and entry](/docs/files-packages-and-entry).
