---
title: "Laws"
description: "Add semantic expectations after class and instance basics."
group: "Abstractions"
section: "Abstractions"
order: 23
slug: "laws"
summary: "Use laws to document the meaning of an abstraction, not just its shape."
---

{{snippet:chapter-laws}}

## In this chapter

Laws document semantic expectations of an abstraction.
In the `Eq` example, `reflexive` states that comparing a value with itself should succeed.
That means class definitions can communicate not only available operations, but also what correct behavior is supposed to preserve.

## Why it matters

Without laws, abstractions can be technically implemented yet still behave in surprising or inconsistent ways.
Users need to know that Musi has a place to state these expectations close to the abstraction itself.
That keeps class design from becoming only a bag of function signatures.

## Walk through it

Read a law as part of abstraction meaning.
It is not there to add another callable member; it tells implementers and readers what behavior should hold across instances.
When adding laws, start with one obvious property that would help another reader tell correct implementation from suspicious one.

## Try it next

- Look at one class operation.
- Write one law that expresses expected behavior of that operation.
- Ask how an instance could violate it.

## Common mistake

Do not treat laws as decorative comments that can say anything without relation to the operations they describe.

## Next

Continue to [Effects](/docs/language/effects-runtime/effects) to shift from shared behavior contracts into explicit requests for external work.
