---
title: "Unions, Variants, and Pattern Matching"
description: "Translate TypeScript discriminated unions into Musi data variants and `match`."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 7
slug: "unions-variants"
summary: "Discriminated union cases map to constructor-style `data` variants and `match` arms."
---

TypeScript discriminated unions give each case a tag:

```typescript
type LoadState =
  | { kind: "loading" }
  | { kind: "loaded"; value: number }
  | { kind: "failed"; message: string };

const state: LoadState = { kind: "loaded", value: 8080 };

const port =
  state.kind === "loaded" ? state.value : 3000;
```

Musi uses variants instead of string tags.

{{snippet:js-ts-union-variant}}

`match` names every shape the code expects. Payload names live with the constructor and come back in the matching arm.
