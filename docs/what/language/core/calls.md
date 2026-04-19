---
title: "Calls"
description: "Learn call syntax as its own reading skill before adding dot-calls."
group: "Core Syntax"
section: "Core Syntax"
order: 10
slug: "calls"
summary: "Call functions directly and follow argument flow left to right."
---
A call is where a named action receives its inputs. The shape is simple: the function name says what will happen, and the arguments say what it happens to.

{{snippet:chapter-calls}}

Calls are easiest to read when the arguments are already named. `sendInvoice(customer, invoice)` tells a clearer story than a call filled with nested construction. When an argument needs work, consider giving that work a name first.

## Call sites are part of the design

The caller is often where an API succeeds or fails. A function that is technically correct but awkward to call will spread awkward code through the package. Prefer argument order that reads like an everyday sentence: actor, object, detail.

## Nested calls

Nested calls are fine when each layer is short. They become hard when the reader has to count parentheses to understand the job. If a line starts to look like a stack of boxes, put one box on the table and name it.

Core forms are the small hand tools of the language. Literals are raw materials, calls ask for work, operators join values, ranges mark spans, and lambdas carry a small action around. Each form should feel boring before you rely on it inside larger data or effect code.

When a core form feels hard, place it in a plain story. A ticket price plus a fee, a room number in a range, a message sent through a callable, or a small action saved as a lambda is enough. The syntax matters because it keeps that story exact.
