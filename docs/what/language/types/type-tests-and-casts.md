---
title: "Type Tests and Casts"
description: "Check and narrow values with explicit type-facing expressions."
group: "Types"
section: "Types"
order: 21
slug: "type-tests-and-casts"
summary: "Use `:?` to test a value against a type and `:?>` for an explicit cast."
---
Type tests and casts are boundary tools. They appear when code receives a value from a less precise place and needs to narrow or reinterpret it carefully.

{{snippet:chapter-type-tests-and-casts}}

Prefer data shapes that avoid casting. When a cast is needed, keep it close to the boundary and name the result so the rest of the program can return to precise types.

## Check before trusting

If a value comes from outside the package, a file, or foreign code, the program may need to verify it. A cast without a nearby reason asks the reader to trust too much.

## Better than scattered casts

Centralize conversions. A payment gateway adapter can turn raw values into typed records once. The rest of the checkout code should not repeat the same uncertainty.

Types are labels that prevent later guesswork. A ticket number, room name, function value, and generic box can all be written as values, but type notes say which promises the code expects to keep.

Do not add type detail as decoration. Add it when it helps a reader, fixes an edge, explains a public surface, or lets generic code say exactly which kind of value it can accept.
