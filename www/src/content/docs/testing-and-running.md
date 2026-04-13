---
title: "Testing and running"
description: "Run a package, run tests, and use the direct CLI when needed."
group: "Tooling"
section: "Tooling"
order: 16
slug: "testing-and-running"
summary: "The main commands for package work and direct file work."
---

Testing and execution are split by scope:
- package scope with `musi`
- direct source/artifact scope with `music`

## Test shape

- keep tests in `*.test.ms`
- expose each test with exported `test`

{{snippet:stdlib-testing-import}}

## Package commands

{{snippet:package-commands}}

## Direct commands

{{snippet:music-direct}}

## Compare

{{example:testing-entry}}

Use `musi` for normal project work. Use `music` when you want to run or inspect one source file or built artifact directly.

## Try it

{{try:testing-and-running}}

## Next step

Run one package command and one direct command, then revisit any chapter where behavior is unclear.

See [Community](/community) for project entry points and the public guestbook.
