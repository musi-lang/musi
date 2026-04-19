---
title: "Literals"
description: "Meet Musi's everyday literal values before mixing them with operators."
group: "Core Syntax"
section: "Core Syntax"
order: 6
slug: "literals"
summary: "Start with numbers, strings, booleans, runes, and template text."
---
Literals are values written directly into the program: numbers, text, booleans, and similar small facts. They are useful for examples, defaults, and obvious constants.

{{snippet:chapter-literals}}

A literal should not hide a business rule. The number `18` in a cinema app may mean adult age, screen number, or row count. A name such as `adultAgeLimit` lets the next reader know which fact the program depends on.

## Everyday facts in code

Text literals make labels, messages, and file paths visible. Numeric literals can express small counts, prices, limits, or indexes. The value is not the whole story; where it is placed tells the reader how it is being used.

## When a literal deserves a name

If changing a literal would change policy, name it. A delivery fee, retry count, or school passing grade should not be scattered through a file as repeated numbers.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
