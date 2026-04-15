---
title: "`let`, `const`, and Mutable State"
description: "Translate JavaScript and TypeScript binding habits into Musi value mutation."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 3
slug: "state"
summary: "`const`-style names stay ordinary; changing values use explicit mutable values."
---

JavaScript separates rebinding from fixed binding with `let` and `const`:

```javascript
let visits = 0;
visits = visits + 1;
visits;
```

Musi keeps the name ordinary and puts mutability on the value.

{{snippet:js-ts-mutable-state}}

Use mutation when the same local value changes over time. Use a new `let` when a step produces a new value.

```javascript
const base = 1200;
const total = base + 45;
total;
```

{{snippet:js-ts-fresh-value}}
