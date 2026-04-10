# Toolchain

**What**: operational guide to Musi command surfaces and workflow boundaries.
**Why**: contributors and tools need one place that explains which command owns which job.
**How**: use this when shaping CLI behavior, package workflow, or project-facing tooling.
**Where**: rationale for the split lives in `docs/why/toolchain-split.md`.

## Command Surfaces

Musi has two command surfaces:

- `music`: direct language and `.seam` tool
- `musi`: package-aware operator tool

They are related, but they do different jobs.

## `music` Flow

`music` owns direct file and artifact work:

- analyze source
- emit `.seam`
- run source or `.seam`
- inspect or disassemble artifacts

Use it when the task is compiler/runtime direct, not package-aware.

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

Schema-owned tooling areas such as fmt/lint/bench/publish/lock stay in the tool ecosystem without becoming compiler-core concerns.

## Entry Selection

Package-aware entry resolution follows:

- explicit CLI target first
- manifest-configured entry next
- package-style fallback such as `index.ms`

Language startup itself still stays top-level-driven.

## See Also

- `docs/why/toolchain-split.md`
- `docs/where/workspace-map.md`
- `docs/where/phase-boundaries.md`
