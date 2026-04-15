---
title: "Modules, Packages, and Imports"
description: "Translate JavaScript and TypeScript module imports into Musi package imports."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 10
slug: "modules-packages"
summary: "ES module imports map to Musi package imports and exported names."
---

JavaScript modules export names:

```javascript
export function defaultPort() {
  return 8080;
}
```

A consumer imports those names:

```javascript
import { defaultPort } from "./ports.js";

const port = defaultPort();
port;
```

Musi exports names from a package file and imports that package as a value.

{{snippet:js-ts-module-export}}

{{snippet:js-ts-module-import}}
