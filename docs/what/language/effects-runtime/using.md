---
title: "Using"
description: "Track required effects with `using`, understand capability flow, and keep effectful code readable."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 26
slug: "using"
summary: "`using` tells readers and the compiler which effects a callable may request."
---
Using an effect means writing code that may request one of its operations. The request is explicit, so the reader can see when the program leaves plain calculation and asks another part of the system for help.

{{snippet:chapter-using}}

This is useful in ordinary programs. A store checkout can calculate totals without effects, but printing the receipt, reading a coupon file, or asking for the current time crosses a boundary.

## Requests in the middle of logic

Keep requests near the reason they are needed. If a function needs the current time to label a delivery estimate, request the time, name it, and then continue with normal values. Do not spread clock requests through every helper.

## Testing code that requests work

Explicit requests make tests easier to explain. A test can provide a handler that answers with a known time, a known random number, or a fake file result. The business rule can then be checked without depending on the real machine.

Effect chapters draw a line between local calculation and outside answers. Adding prices is local. Reading time, asking a process, or writing a log needs an answer from outside the expression.

A useful test is to ask whether the result could be known from the input alone. If yes, keep the function plain. If no, name the request and make the effect boundary visible.
