---
title: "Classes"
description: "Start abstraction work with class declarations alone before instances or laws."
group: "Abstractions"
section: "Abstractions"
order: 21
slug: "classes"
summary: "Describe shared behavior with class declarations."
---

{{snippet:chapter-classes}}

## In this chapter

A class describes a behavior surface for values of some type.
In this example, `Eq[T]` says that values of type `T` can be compared for equality, and the law names one semantic expectation that should hold.
The key idea is contract first: what operations and guarantees exist before any one concrete implementation appears.

## Why it matters

Users coming from inheritance-heavy languages can misread classes immediately.
This page should prevent that by showing classes as behavior descriptions, not object hierarchies.
Once that distinction is clear, later instance and law pages feel like natural follow-ups instead of confusing add-ons.

## Walk through it

Read `let Eq[T] := class { ... };` as definition of shared capability over some type parameter `T`.
Inside it, focus first on operation shape `let (=) ... : Bool;`, then on the law as statement about meaning rather than syntax decoration.
When writing your own first class, keep member count tiny and choose behavior that several concrete types could plausibly share.

## Try it next

- Define one class with one operation.
- Name the behavior after what callers need.
- List one or two concrete types that should satisfy it.

## Common mistake

Do not read Musi classes as inheritance trees with hidden fields or subclass state.

## Next

Continue to [Instances](/docs/language/abstractions/instances) to see how one concrete type fulfills that behavior contract.
