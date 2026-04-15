---
title: "Testing and Tooling"
description: "Translate JavaScript and TypeScript test habits into Musi package tests."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 12
slug: "testing-tooling"
summary: "Use `@std/testing` for small package tests that name behavior and return test results."
---

JavaScript and TypeScript tests usually name behavior, call code, and assert a result:

```typescript
function defaultPort(): number {
  return 8080;
}

test("default port is http alt", () => {
  expect(defaultPort()).toBe(8080);
});
```

Musi uses `@std/testing` for the same shape.

{{snippet:js-ts-testing-tooling}}
