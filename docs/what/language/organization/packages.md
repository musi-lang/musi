---
title: "Packages"
description: "Learn package roots and entry files after single-file work makes sense."
group: "Code Organization"
section: "Code Organization"
order: 16
slug: "packages"
summary: "Move from one file to package-managed code without changing mental models."
---

A package is a project boundary. It gives source files a root, gives tools a manifest, and gives other packages a stable name to import.

{{snippet:chapter-packages}}

## Reading Model

Read the example from top to bottom. The first visible name gives the reader a handle, the following expressions show how values move, and the final expression shows what leaves the example.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Imports and Exports](/learn/book/organization/modules/imports-and-exports).
