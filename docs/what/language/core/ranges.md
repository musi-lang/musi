---
title: "Ranges"
description: "Learn Musi's range operators in isolation before they appear inside larger code."
group: "Core Syntax"
section: "Core Syntax"
order: 8
slug: "ranges"
summary: "Read open, closed, and spread-like range forms without guessing."
---
Ranges describe a span of values. They appear anywhere a program talks about positions, dates, seats, pages, ages, or levels.

{{snippet:chapter-ranges}}

A range is more than two endpoints. The program also needs to know whether the endpoints are included and how values move from one point to the next.

## Real-world spans

A classroom may use seat numbers `1` through `30`. A calendar may use the days from Monday to Friday. A game may allow levels `1` through `10`. Ranges make those shapes visible instead of scattering boundary checks.

## Boundaries need names

When a range encodes policy, name the endpoints. `adultAgeMinimum` is easier to audit than a raw `18`, and `lastPublicPage` is clearer than a number buried inside a comparison.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
