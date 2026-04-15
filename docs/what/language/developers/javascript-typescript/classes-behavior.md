---
title: "Classes, Objects, and Shared Behavior"
description: "Translate JavaScript classes and TypeScript interfaces into Musi classes and instances."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 11
slug: "classes-behavior"
summary: "Use Musi classes for shared behavior contracts, not as object constructors."
---

JavaScript classes often combine construction with behavior:

```javascript
class Car {
  wheels() {
    return 4;
  }
}

const car = new Car();
car.wheels();
```

TypeScript can describe the behavior through an interface:

```typescript
interface Vehicle {
  wheels(): number;
}
```

Musi separates data from shared behavior.

{{snippet:js-ts-class-instance}}

Use `data` for the value shape. Use `class` and `instance` when several types need the same behavior.
