---
title: "Errors, Results, and Effects"
group: "Musi for Developers"
section: "C99 Developers"
order: 8
slug: "errors-results-effects"
---

# Errors, Results, and Effects

C99 often returns `0`, `-1`, or an enum status and asks callers to inspect globals or out-parameters. In Musi, use `result.Result[T, E]` for local failures and effects for host/runtime operations.

{{snippet:c99-result-value}}
