---
title: "Testing and running"
description: "Run a package, run tests, and use the direct CLI when needed."
group: "Tooling"
section: "Tooling"
order: 16
slug: "testing-and-running"
summary: "The main commands for package work and direct file work."
---

## What
Testing and execution are split by scope:
- package scope with `musi`
- direct source/artifact scope with `music`

## When
Use tests for routine verification.
- keep tests in `*.test.ms`
- expose each test with exported `test`

{{snippet:stdlib-testing-import}}

{{snippet:music-direct}}

## Why
This split keeps quick one-off checks and team workflow checks both fast.

## Where
Apply this guidance in modules and packages where this construct appears.

## How
Use package commands in normal development:

{{snippet:package-commands}}

## Compare
{{example:testing-entry}}

## Analogy
Like project commands in a framework plus one-off script execution when needed.

## Try it
Run one package command and one direct command, then revisit any chapter where behavior is unclear.

See [Reference](/reference) for source, grammar, extension, and issue links.
