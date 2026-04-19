---
title: "Operators"
description: "Add operators after literals so expressions stay readable."
group: "Core Syntax"
section: "Core Syntax"
order: 7
slug: "operators"
summary: "Read arithmetic, comparison, and logic in ordinary expressions."
---
Operators are compact names for common operations. Arithmetic, comparison, and logical operators are useful because people already read them quickly.

{{snippet:chapter-operators}}

Compact syntax is only helpful when it stays obvious. A price comparison like `subtotal < limit` is clear. A dense expression with mixed operators can hide the rule it is trying to enforce.

## Make precedence boring

Parentheses are not a failure. If grouping matters to a business rule, show the grouping. A discount formula should not make a reader remember an operator table before trusting the answer.

## Operators and domain language

Some domains naturally use operators: ranges, totals, equality checks, and ordering. For more specific actions, a named function often reads better than a clever operator expression.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
