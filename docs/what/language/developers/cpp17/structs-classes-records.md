---
title: "Structs, Classes, and Records"
description: "Read Structs, Classes, and Records as a C++17 habit shift, with links to the Musi Book definition."
group: "Musi for Developers"
section: "C++ Developers"
order: 5
slug: "structs-classes-records"
summary: "Translate the C++17 habit, then use the Musi Book for the full rule."
---

C++ classes can store fields, enforce invariants, overload operators, and manage resources at once. Musi asks whether this page needs fields, variants, or behavior before choosing records, data, or classes.

{{compare:cpp17-structs-classes-records}}

## Reading Structs, Classes, and Records from C++17

On the Musi side, Musi records and data variants describe stored shape. Behavior belongs in functions or classes/instances, not inside an object by default. Read the shared example through C++17 eyes: keep the useful instinct, then let Musi name shape, behavior, absence, and outside work in separate places.

## False friend

Do not read `class` as object shape. If the page is about a pet, invoice, room, or vehicle, start with fields or variants. For a C++ reader, the trap is turning every Musi value into a class-shaped design; Musi `class` is closer to a concept or trait than a C++ class; records/data store shape, instances satisfy behavior.

## When this pays off

Use records when names like `owner`, `age`, `amount`, or `route` are the point of the value. The C++17 instinct still helps here: Keep the C++ habit of asking which operation is generic and which value owns shape.

## Keep close

- [Records](/learn/book/data/records)
- [Classes](/learn/book/abstractions/classes)
- [Patterns](/learn/book/data/patterns)
