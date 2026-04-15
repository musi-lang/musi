---
title: "Objects, Records, and Field Updates"
description: "Translate object literals, structural types, and object spread into Musi records."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 4
slug: "objects-records"
summary: "Object literals map to record values, and object spread maps to record spread updates."
---

JavaScript object literals carry named fields:

```javascript
const local = {
  host: "localhost",
  port: 8080,
  secure: false,
};

const secure = { ...local, secure: true };
secure.port;
```

TypeScript usually gives that object a named shape:

```typescript
type Endpoint = {
  host: string;
  port: number;
  secure: boolean;
};
```

Musi can name the record-shaped data and use record spread for updates.

{{snippet:js-ts-object-record}}

Read `{ ...local, secure := 0 = 0 }` as "copy the fields from `local`, then replace `secure`."
