---
title: "First program"
description: "Create the smallest useful Musi file and run it with the direct command lane."
group: "Start"
section: "Start"
order: 2
slug: "first-program"
summary: "Write one file, bind one value, and run it end to end."
---

{{snippet:chapter-first-program}}

## In this chapter

A first Musi program can be only a binding and a final expression.
There is no extra ceremony here: no wrapper function, no package manifest, and no boilerplate runtime setup.
The file reads top to bottom, and the last expression is the result you are asking Musi to evaluate.

## Why it matters

This is first meaningful win for new readers.
If the first page immediately adds packages, imports, or larger syntax, beginners stop learning the language and start fighting setup detail.
A two-line program proves the core reading model first: bind a value, then use it.

## Walk through it

Read `let answer := 42;` as "introduce a name for a value I care about."
Read the final `answer;` line as "this is the result of the file."
Once that shape feels normal, you can change the value, add one more binding above it, or swap the final line for a larger expression without changing the mental model.

## Try it next

- Create `index.ms` with one `let` binding.
- End the file with the bound name.
- Run `music check index.ms` to confirm the file shape.

## Common mistake

Do not go hunting for a mandatory `main` just because other languages require one.

## Next

Continue to [Values and let](/docs/language/start/values-and-let) to make that first binding pattern do real work.
