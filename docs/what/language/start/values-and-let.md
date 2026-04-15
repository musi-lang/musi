---
title: "Values and let"
description: "Learn what `let` means in Musi, how names are introduced, and when `let rec` matters."
group: "Start"
section: "Start"
order: 3
slug: "values-and-let"
summary: "Use `let` to name values, define callables, and understand when recursion needs `let rec`."
---

{{snippet:chapter-values-and-let}}

`let` is one of the most important words in Musi.
It introduces a name, and that same surface grows with the language: plain values, functions, methods, and recursive definitions all start from `let`.

## In this chapter

Read `let` as "bind this name to this value or definition."
That is true whether the right-hand side is:

- a number
- a string
- a block
- a function body
- a data definition

This is why Musi feels uniform.
You learn one binding form, then keep extending it instead of switching to a new top-level declaration family every few pages.

You also meet `let rec` here.
Use `let rec` when a binding must refer to itself while it is being defined.
That is the ordinary way to write recursive functions and other self-referential definitions.

## Why it matters

Readers from Python or JavaScript often expect assignment-looking code to mean mutating state.
That is not what plain `let` means in Musi.
Plain `let` introduces a binding.
If you want later mutation, that is a separate choice with `mut`.

Readers from C-like languages also often expect loops to be the default way to repeat work.
Musi does not start there.
Simple repetition often begins with recursion, which is why `let rec` belongs early in the book.

## Walk through it

Read the example in order:

1. a name is introduced with `let`
2. the right-hand side determines what that name means
3. later expressions use that name directly

When you see `let rec`, stop and ask one question: "does this definition need to see itself?"
If yes, `rec` is appropriate.
If not, stay with plain `let`.

A useful comparison for C-style readers is:

- C-like thinking: define control flow first, then push values through it
- Musi thinking: define values and transformations first, then let recursion or data shape drive control flow

## Try it next

- Write one plain `let` binding.
- Turn one helper into `let rec` and make it call itself.
- Explain why that recursive binding needs `rec`.

## Common mistake

Do not treat `let` as hidden mutation.
It introduces a binding.
Mutation only starts when you opt into mutable state.

## Next

Continue to [Blocks and expressions](/docs/language/start/blocks-and-expressions) to see how several local steps still collapse into one final result.
