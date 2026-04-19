---
title: "Tuples and Unit"
description: "Group values by position and recognize the empty value."
group: "Core Syntax"
section: "Core Syntax"
order: 7
slug: "tuples-and-unit"
summary: "Use tuple expressions for small positional groups and unit when no payload matters."
---
Tuples group a few values by position. Unit represents the single value used when the result itself carries no information.

{{snippet:chapter-tuples-and-unit}}

Use a tuple when the grouped values are temporary and the positions are obvious. Coordinates, paired names, and quick internal returns can fit well. Use a record when the fields deserve names.

## Position versus field name

`(left, right)` is fine when the pair lives near the code that creates it. A customer address should not be a long tuple; the fields need names because the reader must know which part is street, city, or postal code.

## Unit in everyday terms

Unit is like a signed delivery receipt that says the action happened, not a box of new data. Printing a line or sending a request may return unit because the important result is outside the value.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
