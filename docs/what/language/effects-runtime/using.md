---
title: "Using"
description: "Track required effects with `using`, understand capability flow, and keep effectful code readable."
group: "Effects and runtime"
section: "Effects and runtime"
order: 25
slug: "using"
summary: "`using` tells readers and the compiler which effects a callable may request."
---

{{snippet:chapter-using}}

`using` is Musi's way of making effect requirements visible.
It answers a practical question: what capabilities may this code request while it runs?

## In this chapter

Read `using { Console }` as part of the callable's public shape.
Effect sets can include named effects and a rest entry such as `...Base` when a signature forwards an existing capability set.
It says this code may request operations from `Console`.
That is not a hidden implementation detail.
It is something callers, handlers, and readers should be able to see.

A callable type can also show the same distinction:

- `T -> U` for pure callables
- `T ~> U` for effectful callables

Those two spellings matter because Musi does not hide effectful work behind the same surface as pure transformation.

## Why it matters

Users coming from mainstream languages often expect effects to be ambient.
File I/O, randomness, or console input might just happen inside the function body with no visible contract.
Musi goes the other direction.
It makes effect use explicit so code review and composition stay honest.

That visibility matters even more when you later reach handlers.
If a request appears, there should be a visible effect story around it.

## Walk through it

Read a `using` example in three parts:

1. which effect set appears in the signature?
2. which `request` expressions appear in the body?
3. who will eventually handle those requests?

If the body requests an effect that is not present in the surrounding `using` set, that is a type error, not a hidden runtime surprise.

## What Musi does not have

Musi does not let effectful work blend into ordinary pure code without a marker.
Instead of hoping readers notice side effects by convention, the language makes capability requirements visible.

## Try it next

- Write one pure callable type with `->`.
- Write one effectful callable type with `~>`.
- Add `using { Console }` to a callable that requests console input.

## Common mistake

Do not read `using` as optional documentation.
It is part of the callable's real contract.

## Next

Continue to [Handlers](/docs/language/effects-runtime/handlers) to see how requested work gets interpreted.
