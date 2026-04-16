---
title: "Errors, Results, and Effects"
group: "Musi for Developers"
section: "C++17 Developers"
order: 8
slug: "errors-results-effects"
---

# Errors, Results, and Effects

C++17 exceptions hide control flow across call boundaries. Musi makes fallibility visible through `result.Result[T, E]` and host operations visible through effects.

{{snippet:cpp17-result-value}}
