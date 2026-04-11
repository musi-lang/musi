# Stack Roadmap

This page tracks architectural completion lanes across compiler, runtime, packages, and foundations.

Unlike `docs/status/feature-matrix.md`, this page is not limited to current implemented-vs-tested truth. It records whether each layer has reached its intended long-term boundary.

## Current Lanes

| Lane                                         | Status | Notes                                                                                                                                                                                                                        |
| -------------------------------------------- | ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Compiler pipeline (`music_*`)                | ✅      | Frontend through SEAM emission is the main backend path                                                                                                                                                                      |
| Runtime execution (`musi_vm` + `musi_rt`)    | ✅      | Canonical SEAM runtime path exists                                                                                                                                                                                           |
| Repo-owned native host layer (`musi_native`) | ✅      | `NativeHost` owns the repo-provided host/world boundary for the supported desktop target set, including test-session collection, platform dispatch, explicit unsupported-target rejection, and embedding fallback delegation |
| Portable first-party packages (`@std/*`)     | ✅      | First-party package family is repo-owned in `packages/std`, compiles through normal project/runtime flow, and carries co-located tests                                                                                       |
| Foundation namespace (`musi:*`)              | ✅      | `musi_foundation` owns the canonical Rust-side registry for the foundation inventory: `musi:test` and `musi:syntax`                                                                                                          |

## Stable Boundaries

- `musi_native` owns repo-provided host/world integration for macOS, Linux, and Windows
- `musi:*` is the source-visible foundation namespace, not STL surface and not compiler-owned package magic
- the foundation inventory is `musi:test` and `musi:syntax`
- `@std/*` stays portable library surface above that foundation
- docs describe one canonical stack from source to runtime to first-party packages without hidden compiler/package magic

## Done Criteria

- `musi_native` fully owns repo-provided host/world integration for the supported desktop target set
- `musi:*` has an explicit foundation inventory and placement rule for what belongs there
- `@std/*` stays portable library surface above that foundation
- docs describe one canonical stack from source to runtime to first-party packages without hidden compiler/package magic

## See Also

- `docs/status/feature-matrix.md`
- `docs/where/stack-map.md`
- `docs/how/runtime/runtime-api.md`
