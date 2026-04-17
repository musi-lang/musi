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

Read the example from top to bottom. The generated package starts with a hello-world entry file and a tiny add test, so checking and testing stay separate from runtime entry conventions.

## Standard Library

Packages use the standard library by default. That default makes imports such as `@std/testing` available without dependency boilerplate. Add `"lib": []` to `musi.json` only when a package needs to opt out of bundled libraries.

## Installed Modules

Package dependencies are materialized under `musi_modules/` by default. The directory is project-local, so Musi packages can live beside Rust, JavaScript, or other language projects without colliding with `target/` or `node_modules/`.

Use `"musiModulesDir": "vendor/musi"` to choose a different local module directory. Use `"musiModulesDir": false` to disable local materialization and load packages directly from the global Musi cache.

Registry and Git dependencies also use a global cache. Set `MUSI_CACHE_DIR` when a machine needs a fixed cache location, such as CI. `musi install` resolves dependencies, hydrates `musi_modules/` when enabled, and writes `musi.lock`.

## Practical Rule

Use this form when it makes value movement clearer than copying habits from another language. Prefer the smallest form that still tells the reader where names, types, effects, and boundaries live.

Continue to [Imports and Exports](/learn/book/organization/modules/imports-and-exports).
