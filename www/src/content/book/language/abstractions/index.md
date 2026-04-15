---
title: "Abstractions"
description: "Learn classes, instances, and laws without object-model confusion."
group: "Abstractions"
section: "Abstractions"
order: 6
slug: "abstractions"
summary: "Separate behavior shape, concrete implementation, and semantic law into distinct chapters."
---

This part moves from plain functions into reusable behavior contracts.
Classes describe shared behavior, instances provide concrete implementations, and laws explain what those abstractions are supposed to mean.
The section works only if each layer stays distinct.

## In this chapter

This section teaches three abstraction layers: contract, implementation, and semantic expectation.
Those layers are connected, but they are not interchangeable.
Understanding the separation is more important than memorizing every keyword.

## Why it matters

Abstraction chapters become overwhelming when readers cannot tell whether they are looking at behavior shape, specific implementation, or mathematical expectation.
Too little explanation here leads straight to "what is class for?" or "where does real behavior live?"
This section should answer those questions with small examples before complexity grows.

## Walk through it

Read class page first, then instance page, then law page.
For each example, ask what stays generic, what becomes concrete, and what property should hold across all correct implementations.
If that separation is blurry, stay with the small `Eq`-style examples until it is not.
