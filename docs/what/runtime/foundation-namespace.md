# Foundation Namespace

This page defines the long-term boundary for `musi:*`.

## What `musi:*` Is

`musi:*` is the source-visible foundation namespace of Musi.

It contains the things every Musi user, including the language developer, can rely on when building everything else in Musi itself.

It is not STL in disguise.

## Why It Exists

Many languages hide foundational behavior behind compiler magic, builtin package names, or special runtime escape hatches.

Musi keeps that foundation source-visible instead.

That means:

- foundational capabilities live in `musi:*`
- ordinary packages stay ordinary packages
- users build upward in Musi itself instead of depending on hidden package/compiler tricks

## What Belongs Here

Something belongs in `musi:*` when it is part of the permanent foundation that higher layers must be able to rely on without compiler-owned package magic.

Typical shape:

- foundational capability roots
- source-visible wiring points that other languages would often hide
- stable substrate that `@std/*` and user packages may build on

## What Does Not Belong Here

These do not belong in `musi:*`:

- convenience standard-library APIs
- portable pure-language helpers
- duplicate `@std/*` family surface
- compiler-only metadata that belongs under `@musi.*`
- host implementation internals that belong in `musi_native`

## Layer Boundaries

- `musi_vm` executes SEAM
- `musi_native` owns repo-provided host/world integration
- `musi_rt` composes source-aware runtime flow
- `musi:*` exposes the foundation above those runtime seams
- `@std/*` builds portable library surface on top of that foundation
- `@musi.*` remains compiler-only attribute space

## Current State

`musi:test` is the currently implemented foundation root.

The broader foundation inventory is not complete yet.

## Done Criteria

`musi:*` is complete when:

- its inventory is explicit
- its placement rule is stable
- its boundary with `@std/*`, `musi_rt`, `musi_native`, and `@musi.*` is documented without overlap
- higher-level packages can be specified against it without hidden compiler/package magic

## See Also

- `docs/where/stack-map.md`
- `docs/why/runtime-boundary.md`
- `docs/how/runtime/runtime-api.md`
- `docs/what/language/compiler-attributes.md`
