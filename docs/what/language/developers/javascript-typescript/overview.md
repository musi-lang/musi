---
title: "Overview"
description: "Translate ES6 JavaScript and TypeScript 5.9 habits into Musi code with side-by-side examples."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 1
slug: "overview"
summary: "Start from ES6 JavaScript runtime habits and TypeScript 5.9 type habits, then read the equivalent Musi shapes."
---

ES6 JavaScript supplies the runtime habits for this guide: objects, functions, arrays, promises, modules, and tests.
TypeScript 5.9 is the typed comparison point for annotations, generic functions, union modeling, and import habits.

Each page starts with JavaScript or TypeScript code, then shows the matching Musi code with the same names and the same job.

## First translation

TypeScript has several function forms. The ordinary declaration reads like this:

```typescript
function total(base: number, fee: number): number {
  return base + fee;
}

const answer = total(1200, 45);
answer;
```

The closer shape to Musi is a named value that stores a function:

```typescript
const total = function (base: number, fee: number): number {
  return base + fee;
};

const answer = total(1200, 45);
answer;
```

Musi keeps the named value shape, but the function body is an expression. The final expression is the result.

{{snippet:js-ts-values-functions}}

## Reading path

- [Values, Functions, and Final Expressions](/learn/book/developers/guides/javascript-typescript/values-functions)
- [`let`, `const`, and Mutable State](/learn/book/developers/guides/javascript-typescript/state)
- [Objects, Records, and Field Updates](/learn/book/developers/guides/javascript-typescript/objects-records)
- [Arrays and Data Pipelines](/learn/book/developers/guides/javascript-typescript/arrays-pipelines)
- [Null, Undefined, Option, and Result](/learn/book/developers/guides/javascript-typescript/null-result)
- [Unions, Variants, and Pattern Matching](/learn/book/developers/guides/javascript-typescript/unions-variants)
- [Generics and Type Parameters](/learn/book/developers/guides/javascript-typescript/generics)
- [Promises, Async, and Effects](/learn/book/developers/guides/javascript-typescript/promises-effects)
- [Modules, Packages, and Imports](/learn/book/developers/guides/javascript-typescript/modules-packages)
- [Classes, Objects, and Shared Behavior](/learn/book/developers/guides/javascript-typescript/classes-behavior)
- [Testing and Tooling](/learn/book/developers/guides/javascript-typescript/testing-tooling)

## Habits that transfer

- keep small functions
- name object fields clearly
- make module boundaries readable
- use type annotations where readers need certainty
- keep side effects at visible edges

## Habits to translate

- `return` becomes the final expression
- object spread becomes record spread
- `null` and `undefined` become explicit data shapes
- TypeScript unions become `data` variants when cases carry meaning
- promises and async work become effect requests at boundaries
