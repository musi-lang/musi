# Toolchain

## Command Surfaces

Musi has two command surfaces:

- `music`: direct language and `.seam` tool
- `musi`: package-aware operator tool

They do different jobs.

## `music` Flow

`music` owns direct file and artifact work:

- `check <source.ms>`
- `build <source.ms> [--out PATH]`
- `run <source.ms|artifact.seam>`
- `inspect <artifact.seam>`
- `disasm <artifact.seam>`

Use it when the task is compiler/runtime direct.

## `musi` Flow

`musi` owns package and operator workflow:

- `new <name>`
- package-aware `check`, `build`, `run`, and `test`
- task execution
- dependency and workspace operations

Use it when manifest or workspace context matters.

## Manifest-Driven Areas

Manifest-backed operator areas include:

- entries
- exports and imports
- dependencies
- tasks
- test configuration
- workspace wiring

Manifest-backed tooling such as fmt/lint/bench/publish/lock stays above compiler-core crates.

## Entry Selection

Package-aware entry resolution follows:

- explicit CLI target first
- manifest-configured entry next
- package-style fallback such as `index.ms`

Package ownership comes from the nearest ancestor `musi.json`.

## Diagnostics Contract

Package-aware editor diagnostics flow through:

- `musi check --diagnostics-format json`

Direct source diagnostics flow through:

- `music check --diagnostics-format json`

Language startup still stays top-level-driven.

## See Also

- `docs/why/toolchain-split.md`
- `docs/where/workspace-map.md`
- `docs/where/phase-boundaries.md`
