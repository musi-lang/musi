---
title: "Expressions and bindings"
description: "Read Musi through <code>let</code>, sequences, and <code>case</code>."
group: "Core language"
section: "Core language"
order: 5
slug: "expressions-and-bindings"
summary: "The base reading model for names, sequences, and branching."
---

Expressions and bindings are the core reading model in Musi. Bind a name, then keep reading downward.

{{snippet:let-binding}}

## Sequencing

{{snippet:sequence}}

`;` separates expressions. Parentheses can group a sequence into one larger expression.

## Branching

{{snippet:case-port}}

`case ... of` handles branching. Match on shape, then return a value from each branch.

## Try it

{{try:expressions-and-bindings}}

## Next step

Read the two snippets, then move to [Operators and literals](/docs/operators-and-literals).
