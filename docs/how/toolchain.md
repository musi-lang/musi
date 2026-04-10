# Toolchain

## Command Surfaces

Long term, Musi has two command surfaces. Today the repo still ships `musi` as the user-facing binary.

- `music`: direct language and `.seam` tool
- `musi`: package-aware operator tool

They do different jobs.

## `music` Flow

`music` owns direct file and artifact work:

- analyze source
- emit `.seam`
- run source or `.seam`
- inspect or disassemble artifacts

Use it when the task is compiler/runtime direct.

## `musi` Flow

`musi` owns package and operator workflow:

- package-aware `run`
- `test`
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

Language startup still stays top-level-driven.

## See Also

- `docs/why/toolchain-split.md`
- `docs/where/workspace-map.md`
- `docs/where/phase-boundaries.md`
