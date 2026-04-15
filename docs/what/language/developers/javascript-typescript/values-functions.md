---
title: "Values, Functions, and Final Expressions"
description: "Translate JavaScript and TypeScript functions into Musi `let` functions."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 2
slug: "values-functions"
summary: "Use `let` for values and functions, and use final expressions instead of `return`."
---

JavaScript can declare a function directly:

```javascript
function total(base, fee) {
  return base + fee;
}

const answer = total(1200, 45);
answer;
```

JavaScript can also bind a function value to a name. That shape is closer to Musi:

```javascript
const total = function (base, fee) {
  return base + fee;
};

const answer = total(1200, 45);
answer;
```

TypeScript adds parameter and result types to either form:

```typescript
const total = function (base: number, fee: number): number {
  return base + fee;
};

const answer = total(1200, 45);
answer;
```

Musi puts the function in a `let` binding and writes the result type after the parameters.

{{snippet:js-ts-values-functions}}

The last expression leaves the value. There is no `return` keyword in ordinary Musi function bodies.

## Named calls

JavaScript and TypeScript often use an options object when a call has several values:

```typescript
function render(input: { port: number; secure: boolean }): number {
  return input.port;
}

const selected = render({ port: 8080, secure: true });
selected;
```

Musi can name arguments at the call site without wrapping them in an object.

{{snippet:js-ts-named-calls}}
