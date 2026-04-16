---
title: "Null, Option, and Result"
group: "Musi for Developers"
section: "C99 Developers"
order: 7
slug: "null-option-result"
---

# Null, Option, and Result

C99 uses `NULL`, sentinel values, and integer return codes for absence and failure. Musi uses `option.Option[T]` for absence and `result.Result[T, E]` for fallible values.

{{snippet:c99-null-option}}
