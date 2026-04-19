---
title: "Methods and Receiver Calls"
description: "Read Methods and Receiver Calls as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 11
slug: "methods-and-receiver-calls"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

A C++ reader brings habits from RAII, overload sets, templates, references, optional values, exceptions, and object hierarchies. That helps with ownership stories, value categories, and compile-time abstractions, but the Musi page asks a narrower question: what contract should this local values and calls example make visible?

{{compare:cpp17-methods-and-receiver-calls}}

## Reading Methods and Receiver Calls from C++17

On the Musi side, Musi lets a calculation be a `let`, a function, or a final expression without wrapping it in an object or framework first. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not preserve call ceremony just because the source language needs it for methods, receivers, overloads, or closures. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use this shape when a receipt total, label, distance, or score is pure data moving through named calls. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Calls](/learn/book/core/calls)
- [Dot calls](/learn/book/core/dot-calls)
- [Functions](/learn/book/core/functions)
