---
title: "First program"
description: "Write a small file and read it as expressions."
group: "Start"
section: "Start"
order: 2
slug: "first-program"
summary: "A first Musi file without extra ceremony."
---

Musi files are read as expressions. A file is executed from top to bottom as a sequence.

## What
This page teaches the smallest runnable surface: values, bindings, and simple expressions.

{{snippet:first-file}}

## When
Use this pattern for notes, toy utilities, and onboarding examples before you add package config.

## Why
Short files are the fastest way to learn language behavior without project scaffolding.
You can spot how names and results flow before layering packages, effects, or classes.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Use `let` for values and end each expression with `;`. After a value exists, the next expression can use it.

## Compare
{{example:double-function}}

The next snippet introduces a reusable function and direct call style.

## Analogy
Think like Python or JavaScript REPL cells: each statement creates a value, and later statements can use earlier results.

## Try it
Define the two snippets above, evaluate with `music`, then move to [Files, packages, and entry](/docs/files-packages-and-entry).
