---
title: "Variables and Mutation"
group: "Musi for Developers"
section: "C99 Developers"
order: 4
slug: "variables-mutation"
---

# Variables and Mutation

C99 locals are mutable by default. Musi locals are stable by default and use `mut` where mutation is intentional.

{{snippet:c99-variables-mutation}}

This makes mutation stand out the same way pointer writes and out-parameters stand out in careful C.
