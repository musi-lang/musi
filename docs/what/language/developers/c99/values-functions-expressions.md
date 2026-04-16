---
title: "Values, Functions, and Expressions"
group: "Musi for Developers"
section: "C99 Developers"
order: 2
slug: "values-functions-expressions"
---

# Values, Functions, and Expressions

C99 separates declarations, statements, and expressions. Musi leans harder into expressions: blocks can produce values, conditionals can choose values, and functions return the last expression when that is clearer than an explicit temporary.

{{snippet:c99-values-functions}}

Use annotations where the public shape matters. Let inference handle short private bindings.
