---
title: "Values and Let"
description: "Learn what `let` means in Musi, how names are introduced, and when `let rec` matters."
group: "Start"
section: "Start"
order: 4
slug: "values-and-let"
summary: "Use `let` to name values, define callables, and understand when recursion needs `let rec`."
---
Values are the nouns of a Musi program. A `let` declaration gives one of those nouns a stable name so later code can talk about it without repeating the whole expression. If a program is a small cafe, `menuPrice`, `tableNumber`, and `receiptText` are easier to follow than the arithmetic and string building repeated everywhere.

{{snippet:chapter-values-and-let}}

Names are also a way to mark intent. A number named `3` tells you almost nothing; a value named `retryLimit` tells you how the number is used. That makes the compiler's work easier to explain and a reader's work much easier to finish.

## When the name refers back to itself

Some values are defined in terms of a smaller version of the same problem. Counting boxes in a nested shipment, walking a family tree, or computing a countdown all have that shape. Musi uses `let rec` when a declaration needs to call itself.

{{snippet:recursive-case}}

Recursive declarations need a visible smaller step. If each call gets closer to the simple case, the definition reads like a set of instructions. If the next call repeats the same input, the program is like a delivery driver circling the same block.

Read this chapter like a short errand. First find the names, then find the value each name holds, then ask what the last expression gives back. A cafe ticket, a room card, a pet tag, and a bus route all become easier when each useful fact has a clear name.

A common mistake is to race toward large examples. Stay small until the form feels normal. One file, one value, one block, and one changed value teach more than a page full of clever tricks.
