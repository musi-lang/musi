---
title: "Generics and Type Parameters"
description: "Translate TypeScript generic functions and generic objects into Musi type parameters."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 8
slug: "generics"
summary: "TypeScript type parameters map to Musi bracketed type parameters."
---

TypeScript generic functions keep one implementation for many input types:

```typescript
function identity<T>(input: T): T {
  return input;
}

const port = identity<number>(8080);
port;
```

Musi uses bracketed type parameters on the function name.

{{snippet:js-ts-generic-function}}

Generic data uses the same bracketed type parameter shape.

```typescript
type Box<T> = {
  value: T;
};

const boxed: Box<number> = { value: 8080 };
boxed.value;
```

{{snippet:js-ts-generic-data}}
