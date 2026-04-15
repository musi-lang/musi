---
title: "Promises, Async, and I/O Boundaries"
description: "Translate JavaScript promises and TypeScript async boundaries into Musi stdlib I/O."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 9
slug: "promises-effects"
summary: "Boundary work that JavaScript models with promises usually starts at `@std/io` in Musi."
---

JavaScript uses promises for input that arrives from outside the current stack:

```javascript
async function readName(consoleApi) {
  const name = await consoleApi.readLine("name> ");
  console.log(name.trim());
}
```

TypeScript adds the promised result type:

```typescript
async function readName(consoleApi: ConsoleApi): Promise<void> {
  const name: string = await consoleApi.readLine("name> ");
  console.log(name.trim());
}
```

Musi code normally reaches for the standard library wrapper. `@std/io` sits above the runtime request and gives user code stable names.

{{snippet:js-ts-promise-effect}}

Use stdlib modules when they exist. Define custom effects when you are designing a capability that the standard library does not already name.
