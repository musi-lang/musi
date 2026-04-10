# Toolchain Split

**What**: rationale for keeping `music` and `musi` separate.
**Why**: compiler/runtime work and package/operator work have different responsibilities and should not collapse into one giant surface.
**How**: use this when shaping CLI commands, manifest behavior, or project-facing workflow.
**Where**: operational command flow lives in `docs/how/toolchain.md`.

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

The split keeps those concerns explicit and easier to evolve.

## Why Schema-Owned Tooling Still Belongs Here

Manifest-backed areas such as fmt/lint/bench/publish/lock belong to the Musi tool ecosystem.

That does not mean they belong inside compiler-core crates.

## See Also

- `docs/how/toolchain.md`
- `docs/where/workspace-map.md`
- `docs/why/compiler-architecture.md`
