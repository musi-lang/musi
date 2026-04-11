# Stack Roadmap

This page tracks architectural completion lanes across compiler, runtime, packages, and foundations.

Unlike `docs/status/feature-matrix.md`, this page is not limited to current implemented-vs-tested truth. It records whether each layer has reached its intended long-term boundary.

## Current Lanes

| Lane                                         | Status | Notes                                                                                                                                  |
| -------------------------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------- |
| Compiler pipeline (`music_*`)                | ✅      | Frontend through SEAM emission is the main backend path                                                                                |
| Runtime execution (`musi_vm` + `musi_rt`)    | ✅      | Canonical SEAM runtime path exists                                                                                                     |
| Repo-owned native host layer (`musi_native`) | 🟡      | Default runtime integration exists; the full official host/world boundary and target-coverage policy are not complete yet              |
| Portable first-party packages (`@std/*`)     | ✅      | First-party package family is repo-owned in `packages/std`, compiles through normal project/runtime flow, and carries co-located tests |
| Foundation namespace (`musi:*`)              | 🟡      | `musi:test` exists, but the full source-visible foundation that replaces hidden compiler/package magic is not complete yet             |

## Near-Term Focus

- complete the documented host/world boundary in `musi_native`
- define the full long-term contract for `musi:*` as foundational surface, not STL surface
- keep `@std/*` above the foundation as portable library surface rather than builtin-package magic
- document capability boundaries between `musi:*`, `@std/*`, `musi_rt`, `musi_native`, and `@musi.*`

## Done Criteria

- `musi_native` fully owns repo-provided host/world integration for the supported target set
- `musi:*` has an explicit foundation inventory and placement rule for what belongs there
- `@std/*` stays portable library surface above that foundation
- docs describe one canonical stack from source to runtime to first-party packages without hidden compiler/package magic

## See Also

- `docs/status/feature-matrix.md`
- `docs/where/stack-map.md`
- `docs/how/runtime/runtime-api.md`
