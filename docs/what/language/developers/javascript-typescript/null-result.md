---
title: "Null, Undefined, Option, and Result"
description: "Translate nullable JavaScript and TypeScript values into stdlib Option and Result values."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 6
slug: "null-result"
summary: "`null` and `undefined` habits become `@std/option` values when absence matters."
---

JavaScript commonly uses `null` when a value is missing:

```javascript
function findPort(name) {
  return name === "local" ? 8080 : null;
}

const port = findPort("local");
port;
```

TypeScript makes that absence visible in the type:

```typescript
function findPort(name: string): number | null {
  return name === "local" ? 8080 : null;
}
```

Musi uses `@std/option` for ordinary absence. The type names the possibility of no value, and the helper chooses the fallback.

{{snippet:js-ts-null-option}}

For recoverable failure with a reason, use `@std/result` instead of making another result-shaped type.

{{snippet:js-ts-result-data}}
