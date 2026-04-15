---
title: "Instances"
description: "Learn instance declarations only after classes make sense on their own."
group: "Abstractions"
section: "Abstractions"
order: 22
slug: "instances"
summary: "Attach concrete behavior to concrete types."
---

{{snippet:chapter-instances}}

## In this chapter

An instance says how one concrete type satisfies one class.
Where the class page defined behavior shape, this page fills in actual implementation for `Eq[Int]`.
That split is important because it keeps reusable abstraction separate from concrete decision.

## Why it matters

If users only see class declarations, they still ask where the real behavior lives.
If they see classes and instances collapsed together too early, they lose the difference between contract and implementation.
A tiny `Int` instance makes the handoff between those layers easy to follow.

## Walk through it

Read `instance Eq[Int]` as commitment to implement `Eq` behavior for the specific type `Int`.
Inside the block, compare member names with class definition and notice that instance must satisfy required surface.
When writing your own first instance, pick one small class and one obvious concrete type so the mapping from contract to implementation is immediate.

## Try it next

- Take one small class definition.
- Add one instance for `Int` or another simple type.
- Use same member names so relation stays obvious.

## Common mistake

Do not define several classes and several instances at once before one mapping feels clear.

## Next

Continue to [Laws](/docs/language/abstractions/laws) to capture what those abstractions are supposed to mean, not only how they are spelled.
