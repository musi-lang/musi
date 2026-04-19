---
title: "Testing"
description: "Teach tests before the wider command surface so the workflow stays concrete."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 33
slug: "testing"
summary: "Write small package tests that read like ordinary code."
---
Tests describe examples the program must continue to satisfy. A good test reads like a small story: given this cart, the total is this; given this animal age, the category is this.

{{snippet:chapter-testing}}

Tests are not separate from design. Code that is hard to test often has hidden boundaries, vague names, or too many responsibilities in one function.

## Test the rule, not the machinery

If a rule calculates late fees, test the dates and expected fee. Do not make the test depend on today's clock unless the clock behavior itself is the subject.

## Use real examples with small numbers

Everyday examples make failures easier to understand. A failed test about two tickets and one discount is easier to debug than a test full of anonymous placeholders.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
