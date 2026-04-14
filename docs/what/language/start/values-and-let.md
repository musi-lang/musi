---
title: "Values and let"
description: "Learn Musi's core binding form before adding more syntax."
group: "Start"
section: "Start"
order: 3
slug: "values-and-let"
summary: "Bind names with let and read the file top to bottom."
---

{{snippet:chapter-values-and-let}}

## What

`let` is Musi's everyday binding form.
You use it for plain values, and later you will see same starting shape again for functions, methods, modules, and other definitions.
That makes `let` worth learning deeply instead of treating it as throwaway beginner syntax.

## Why

Users ask "how do I name a value and reuse it?" before they ask bigger questions.
A good answer is not only syntax shape, but also why the shape stays important as the language grows.
Once `let` feels ordinary, many later chapters stop looking like brand-new grammar and start looking like familiar definitions with more detail attached.

## How

Read `let port := 8080;` as one stable binding between a name and a value.
Keep the name close to where you will use it so the file stays readable without scrolling around for context.
Then end with `port;` or derive one more binding from it so you can feel the difference between introducing a value and using one.

## Try it

- Bind one number with `let`.
- Add a second binding derived from first.
- End the file with derived value.

## Common mistake

Do not assume `let` is only for top-level constants and not for the rest of language surface.

## Next

Continue to [Blocks and expressions](/docs/language/start/blocks-and-expressions) to see how several bindings can still produce one final value.
