---
title: "Values and Let"
description: "Learn what `let` means in Musi, how names are introduced, and when `let rec` matters."
group: "Start"
section: "Start"
order: 3
slug: "values-and-let"
summary: "Use `let` to name values, define callables, and understand when recursion needs `let rec`."
---

`let` introduces a name. That one idea carries plain values, functions, data definitions, effects, classes, and many other declarations.

{{snippet:chapter-values-and-let}}

Read `let port := 8080;` as "bind the name `port` to this value." The later binding can use it because the first binding is already in scope. Nothing here mutates `port`; a new name receives the next value.

A spreadsheet analogy helps: one cell names an input, another cell derives from it, and the formula tells you which value depends on which.

## Recursive Names

Use `let rec` when a definition must refer to itself while being defined. That is how repeated work can be expressed without making loops the first tool.

{{snippet:recursive-case}}

`loop` can call itself because the definition starts with `let rec`. Read the match arms as the two roads through the calculation: stop at zero, or call `loop` again with a smaller number.

Continue to [Blocks and Expressions](/learn/book/start/foundations/blocks-and-expressions).
