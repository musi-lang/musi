---
title: "Testing"
description: "Teach tests before the wider command surface so the workflow stays concrete."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 33
slug: "testing"
summary: "Write small package tests that read like ordinary code."
---

{{snippet:stdlib-testing-import}}

## What

Musi tests live in `*.test.ms` files and usually export a `test` binding.

## Why

Beginners should see that tests are ordinary code, not a second language.

## How

- Create a `*.test.ms` file.
- Import `@std/testing`.
- Export one small test binding.

## Try it

- Create one test file.
- Write one passing test.
- Run `musi test`.

## Common mistake

Do not wait for a large project before adding tests.

## Next

Continue to [Running and tooling](/docs/language/advanced/running-and-tooling).
