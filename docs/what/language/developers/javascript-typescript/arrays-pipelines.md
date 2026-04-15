---
title: "Arrays and Data Pipelines"
description: "Translate JavaScript array helper habits into Musi pipeline-friendly code."
group: "Musi for Developers"
section: "JavaScript and TypeScript Developers"
order: 5
slug: "arrays-pipelines"
summary: "Arrays stay data; pipeline calls keep transformations readable from left to right."
---

JavaScript often chains array helpers:

```javascript
const ports = [3000, 8080, 9000];
const visible = ports.map((port) => port + 1);
visible;
```

Musi keeps the array value visible and can pipe it through library functions.

{{snippet:js-ts-arrays-pipelines}}

Use a pipeline when the output of one step becomes the input to the next. Use a named `let` when the step needs a domain name.
