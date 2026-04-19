---
title: "Types"
description: "Add type information, understand inference, and write small generic helpers."
group: "Types"
section: "Types"
order: 5
slug: "types"
summary: "Introduce types gradually: explicit first, inferred second, generic third."
---

Types are the promises Musi code makes about values. Some promises are written down as annotations. Some are inferred from nearby code. Some become generic so the same shape can work for many value types.

Read this part after the basic expression and data chapters. Types make more sense when you already have values to talk about: a `Person`, a `Payment`, a `Cart`, a `Clock`, a `Route`, or a `Result`. The annotation is not decoration. It tells the compiler and the next reader what kind of value belongs at that point.

Use annotations where a boundary matters. Exported functions, effect operations, class members, and reusable helpers should say what they accept and return. Use inference where the answer is close and obvious. A short local value built from a clear expression often does not need extra noise.

Generics and callable types are about reusable shape. A box can hold different things. A filter can accept different rules. A result can describe success and failure for many domains. Advanced forms such as `forall` and dependent types appear later because they are strongest when the ordinary type story is already stable.

By the end of this part, you should know how to make type information helpful instead of loud. Good types explain the edge of a function, the shape of a collection, and the reason a helper can be reused.
