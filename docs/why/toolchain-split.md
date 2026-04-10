# Toolchain Split

This page explains why the long-term tool split is `music` + `musi`.

The current repo still ships `musi` as the user-facing binary. This page is about the target split, not the current release surface.

## Why `music` Exists

`music` is the direct language and artifact tool.

It should stay focused on:

- source analysis
- emission
- runtime execution of source or `.seam`
- artifact inspection

## Why `musi` Exists

`musi` is the package-aware operator tool.

It should own:

- manifest-aware entry selection
- tasks
- tests
- dependency workflows
- workspace behavior

## Why They Should Not Collapse

One giant tool surface would blur:

- compiler responsibilities
- runtime responsibilities
- package-manager responsibilities
- workflow/operator responsibilities

The split keeps those concerns separate.

## Why Schema-Owned Tooling Still Belongs Here

Manifest-backed areas such as fmt/lint/bench/publish/lock belong in tooling, not compiler-core crates.

## See Also

- `docs/how/toolchain.md`
- `docs/where/workspace-map.md`
- `docs/why/compiler-architecture.md`
