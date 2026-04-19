---
title: "Classes"
description: "Start abstraction work with class declarations alone before instances or laws."
group: "Abstractions"
section: "Abstractions"
order: 21
slug: "classes"
summary: "Describe shared behavior with class declarations."
---
Classes describe capabilities a type can provide. They are useful when several types can answer the same question in their own way: comparing people by age, formatting receipts, or advancing different kinds of counters.

{{snippet:chapter-classes}}

A class is not a bag of random helpers. It should name one coherent ability. If the methods do not belong in the same sentence, they probably do not belong in the same class.

## Capability over category

Think “can be ordered” or “can be shown,” not “all things my app knows about.” A car, a dog, and an appointment may all be printable, but that does not make them the same kind of object.

## Small interfaces age better

A small class is easier to implement correctly and easier to test. Add methods because the capability needs them, not because a nearby type happens to have them.

Abstraction chapters explain shared behavior. A record says what fields a value has. A class says what a type can do. An instance gives that behavior for one type. A law names a promise callers depend on.

The false friend is the word class. In Musi it is not an object blueprint. Use records and data for shape. Use classes, instances, and laws when many shapes need to share behavior.
