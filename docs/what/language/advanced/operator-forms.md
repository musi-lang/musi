---
title: "Operator forms"
description: "Name operators, set fixity, and call an operator when that reads better."
group: "Advanced and tooling"
section: "Advanced and tooling"
order: 32
slug: "operator-forms"
summary: "Use fixity declarations and parenthesized operator names for advanced operator-heavy code."
---

{{snippet:chapter-operator-forms}}

## In this chapter

Operators can appear as ordinary infix syntax, and parenthesized operator names can be used as values.
Fixity declarations such as `infixl 6 (+);` describe how an operator groups with neighboring expressions.
Most code does not need custom fixity, but operator-heavy libraries do.

## Why it matters

A small amount of operator syntax can make numeric or parser-like code clearer.
Too much can make code unreadable.
Naming the operator with `(+)` and declaring fixity explicitly keeps the unusual parts visible.

## Walk through it

Read `infixl 6 (+);` as a declaration about grouping.
Read `let add := (+);` as binding the operator itself to a name.
Read `add(1, 2)` as an ordinary call through that binding.

## Try it next

- Bind one operator with its parenthesized name.
- Call it like a function.
- Avoid custom fixity unless repeated infix use actually becomes clearer.

## Common mistake

Do not use custom operators where a named function would explain the domain better.

## Next

Continue to [Quote and syntax](/docs/language/advanced/quote-and-syntax) for code-as-data tools.
