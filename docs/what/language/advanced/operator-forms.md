---
title: "Operator Forms"
description: "Name operators, set fixity, and call an operator when that reads better."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 32
slug: "operator-forms"
summary: "Use fixity declarations and parenthesized operator names for advanced operator-heavy code."
---
Operator forms let a language give compact syntax to operations that are common enough to deserve it. They are advanced because compact code can become unclear quickly.

{{snippet:chapter-operator-forms}}

Use operator forms where the domain already has a strong written convention. Arithmetic, ranges, equality, and ordering are easy examples.

## Compact is not always clearer

A named function is often better for a business action. `approveLoan(application)` says more than a custom symbol most readers have never seen.

## Formatting matters

Operators are also visual structure. Spacing and line breaks should reveal grouping, especially in long formulas or match guards.

If an operator form makes the formatter choose strange line breaks, that is a sign to revisit the shape. Operators should help a reader scan the code; they should not force the reader to decode layout before understanding the rule. Long domain rules often deserve named helpers even when an operator is available.

Advanced chapters are tools for edges: metadata, native calls, compile-time work, syntax values, and command-line use. They matter most when ordinary declarations are no longer enough to describe a boundary.

Use advanced forms like locked cabinets, not like kitchen drawers. Reach for them when a tool, runtime, foreign library, or build step truly needs the extra signal. Keep normal program logic in normal declarations.
