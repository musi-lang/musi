---
title: "Using"
description: "Track required effects with `using`, understand capability flow, and keep effectful code readable."
group: "Effects and Runtime"
section: "Effects and Runtime"
order: 25
slug: "using"
summary: "`using` tells readers and the compiler which effects a callable may request."
---

`using` lists the capabilities a function may request. It belongs in the function surface so callers can see the boundary before reading the body.

{{snippet:chapter-using}}

Read `using { Clock }` as a permit list. The body requests clock work, and the signature tells that story before the body starts.

Callable types show the same idea: `T -> U` is pure shape, while `T ~> U` can require effects. Musi does not let effectful work blend into ordinary pure code without a marker.

Continue to [Handlers](/learn/book/effects-runtime/handling/handlers).
