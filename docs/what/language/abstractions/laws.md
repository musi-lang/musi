---
title: "Laws"
description: "Add semantic expectations after class and instance basics."
group: "Abstractions"
section: "Abstractions"
order: 23
slug: "laws"
summary: "Use laws to document the meaning of an abstraction, not just its shape."
---

Laws document the meaning of a class, not just member names. They tell readers what a correct instance must keep true.

{{snippet:chapter-laws}}

A car is a vehicle, but a law can still require at least four wheels before that car counts as valid in the model. The class names the family, the instance describes one member, and the law states the trust rule.

Write a law when it helps separate a believable implementation from a suspicious one.

Continue to [Effects](/learn/book/effects-runtime/handling/effects).
