---
title: "Instances"
description: "Learn instance declarations only after classes make sense on their own."
group: "Abstractions"
section: "Abstractions"
order: 22
slug: "instances"
summary: "Attach concrete behavior to concrete types."
---
Instances connect a class to a specific type. If a class says “this kind of value can be compared,” an instance explains how `Int`, `Person`, or `DeliveryTime` performs that comparison.

{{snippet:chapter-instances}}

Instances are where domain rules become concrete. Comparing people by name is different from comparing them by age. Comparing orders by creation time is different from comparing them by total price.

## Choose the rule deliberately

Do not hide surprising policy in an instance. If there are several reasonable orderings for a type, name the one you are using or make the choice explicit at the call site.

## Keep implementations boring

An instance should mostly translate the class operation into the type's own fields and helpers. If it starts coordinating unrelated services, the abstraction boundary is in the wrong place.

Abstraction chapters explain shared behavior. A record says what fields a value has. A class says what a type can do. An instance gives that behavior for one type. A law names a promise callers depend on.

The false friend is the word class. In Musi it is not an object blueprint. Use records and data for shape. Use classes, instances, and laws when many shapes need to share behavior.
