---
title: "Instances"
description: "Learn instance declarations only after classes make sense on their own."
group: "Abstractions"
section: "Abstractions"
order: 22
slug: "instances"
summary: "Attach concrete behavior to concrete types."
---

An instance supplies class behavior for one concrete type. It bridges a generic behavior contract and a specific implementation.

{{snippet:chapter-instances}}

Read `instance Eq[Int]` as equality behavior for `Int`. The class is the rulebook; the instance is one team showing how it follows the rulebook.

Keep instances predictable and close to the types or classes they explain.

Continue to [Laws](/learn/book/abstractions/classes-instances-laws/laws).
