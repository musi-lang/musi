---
title: "Stdlib"
description: "Place the standard library on top of foundation and runtime so the layering stays clear."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 30
slug: "stdlib"
summary: "Reach for @std modules first in ordinary application code."
---
The standard library gives common names to common shapes. It is where code should find option values, result values, text helpers, collections, and everyday building blocks instead of redefining them in every package.

{{snippet:chapter-stdlib}}

Using the standard library keeps examples and projects speaking the same dialect. A reader should not have to learn a new `Maybe`, `Outcome`, or `TextResult` type in every guide.

## Shared vocabulary

`Option` says a value may be absent. `Result` says an operation may succeed or fail. Those names are small, but they carry a lot of shared expectation. Reuse them when they match the idea.

## Do not copy the library into examples

Examples should teach the feature under discussion, not rebuild the world around it. If an example needs optional data, import or use the standard optional shape. If it needs failure, use the standard result shape.

Effect chapters draw a line between local calculation and outside answers. Adding prices is local. Reading time, asking a process, or writing a log needs an answer from outside the expression.

A useful test is to ask whether the result could be known from the input alone. If yes, keep the function plain. If no, name the request and make the effect boundary visible.
