---
title: "Type Inference"
description: "Learn how inference reduces repetition without hiding too much."
group: "Types"
section: "Types"
order: 19
slug: "type-inference"
summary: "See what Musi can infer so you know when to write less."
---
Type inference lets the compiler fill in obvious types from nearby code. It saves noise when the expression already proves what the type must be.

{{snippet:chapter-type-inference}}

Inference should make code quieter, not mysterious. If a reader has to jump through several files to know what a name is, add the annotation.

## Where inference feels natural

Local totals, temporary strings, and values created from clear constructors usually infer well. A receipt line built from known text does not need to announce itself at every step.

## Where inference should stop

Exports, callbacks, generic helpers, and effect declarations are public or abstract enough that written types help. The compiler may not need the annotation, but the reader and editor often do.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
