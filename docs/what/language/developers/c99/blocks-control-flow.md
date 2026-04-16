---
title: "Blocks and Control Flow"
group: "Musi for Developers"
section: "C99 Developers"
order: 3
slug: "blocks-control-flow"
---

# Blocks and Control Flow

C99 `if`, `for`, and `while` forms map to Musi control flow, but Musi blocks can be value-producing. Prefer `match` when C code would use a `switch` plus sentinel defaults.

{{snippet:c99-block-expression}}
