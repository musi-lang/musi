---
title: "Operator Forms"
description: "Name operators, set fixity, and call an operator when that reads better."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "operator-forms"
summary: "Use fixity declarations and parenthesized operator names for advanced operator-heavy code."
---

{{snippet:chapter-operator-forms}}

## Boundary Tool

Operators can appear as ordinary infix syntax, and parenthesized operator names can be used as values.
Fixity declarations such as `infixl 6 (+);` describe how an operator groups with neighboring expressions.
Most code does not need custom fixity, but operator-heavy libraries do.

## When to Reach for It

A small amount of operator syntax can make numeric or parser-like code clearer.
Too much can make code unreadable.
Naming the operator with `(+)` and declaring fixity explicitly keeps the unusual parts visible.

## Read the Boundary

Read `infixl 6 (+);` as a declaration about grouping.
Read `let add := (+);` as binding the operator itself to a name.
Read `add(1, 2)` as an ordinary call through that binding.

## Small Exercise

- Bind one operator with its parenthesized name.
- Call it like a function.
- Avoid custom fixity unless repeated infix use actually becomes clearer.

## Mistake to Avoid

Do not use custom operators where a named function would explain the domain better.

## Next Page

Continue to [Quote and syntax](/learn/book/advanced/quote-and-syntax) for code-as-data tools.
