---
title: "Testing"
description: "Teach tests before the wider command surface so the workflow stays concrete."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 33
slug: "testing"
summary: "Write small package tests that read like ordinary code."
---

{{snippet:chapter-testing}}

## What

Musi tests are ordinary code organized for discovery and execution by tooling.
This example keeps that promise visible: import testing helpers, export a `test` binding, and express one expectation in same language surface you already know.
Testing becomes easier to adopt when it does not require a second mini-language.

## Why

Users need confidence loop, not just syntax reference.
If the docs explain features but never show how to check them, learners still ask how to verify a package change or protect against regressions.
A tiny test example gives them a habit they can keep using as code grows.

## How

Read `let Testing := import "@std/testing";` as setup of helpers, then read exported `test` binding as entry point tooling will discover.
The assertion itself is ordinary function-style code, which means testing builds on same import, call, and expression patterns from earlier chapters.
When writing first tests, keep each one tiny and named around one behavior you want confidence in.

## Try it

- Create one `*.test.ms` file.
- Export one `test` binding.
- Check one small behavior with stdlib testing helper.

## Common mistake

Do not wait for a large project before learning the test shape; tiny examples benefit from it too.

## Next

Continue to [Running and tooling](/docs/language/advanced/running-and-tooling) to tie learning back to everyday commands.
