# Stack Roadmap

This page tracks the current lanes across compiler, runtime, packages, and capabilities.

## Current Lanes

| Lane                                         | Status | Notes                                                                                                                                                     |
| -------------------------------------------- | ------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Compiler pipeline (`music_*`)                | ✅      | Frontend through SEAM emission is the main backend path                                                                                                   |
| Runtime execution (`musi_vm` + `musi_rt`)    | ✅      | Canonical SEAM runtime path exists                                                                                                                        |
| Repo-owned native host layer (`musi_native`) | 🟡      | Default host adapter exists; broader OS/libc coverage still needs expansion                                                                               |
| Portable first-party packages (`@std/*`)     | 🟡      | First-party package family is now repo-owned in `packages/std`; coverage and tests still need modernization                                               |
| Intrinsic capability namespace (`musi:*`)    | 🟡      | `musi:test` is implemented as the first compiler/runtime-owned capability root; broader host/runtime capability coverage still needs design and execution |

## Near-Term Focus

- modernize `@std/*` package tests against the current toolchain/runtime shape
- keep `musi:*` narrow and capability-oriented while expanding practical runtime foundations
- expand `musi_native` from registered handler adapter into broader official host integration
- document capability boundaries between `musi:*`, `@std/*`, `musi_rt`, and `musi_native`

## Done Criteria

- `@std/*` compiles and tests through normal project/runtime flow
- `@std/*` wraps `musi:*` capabilities where platform-backed standard modules are needed
- docs describe one canonical stack from source to runtime to first-party packages

## See Also

- `docs/status/feature-matrix.md`
- `docs/where/stack-map.md`
- `docs/how/runtime/runtime-api.md`
