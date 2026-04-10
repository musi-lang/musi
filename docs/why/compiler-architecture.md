# Compiler Architecture

## Core Principle

Each layer should own one transformation or one runtime responsibility.

That keeps the system honest:

- language docs define surface
- sema defines meaning
- SEAM defines executable contract
- runtime defines execution and embedding

## Why The Split Exists

The split prevents common failure modes:

- parser or lowering code inventing semantic meaning
- emitter or VM recovering from sema mistakes
- bytecode transport redefining runtime semantics
- tooling concerns leaking into compiler-core crates

## Why SEAM Sits In The Middle

SEAM is the handoff boundary between compile-time meaning and runtime execution.

That gives the pipeline:

- a stable executable contract
- a transportable artifact format
- a clean runtime boundary

## Why Runtime Stays Separate

`musi_vm` executes SEAM and exposes low-level embedding APIs.

`musi_rt` sits above that boundary and composes `music_session` with `musi_vm` for source-backed runtime services.

It should not become:

- a second semantic phase
- a package manager
- a source-language recovery layer

## Why Tooling Stays Above

`music` and `musi` sit above the phase chain.

They orchestrate and present the system. They do not redefine ownership inside it.

## See Also

- `docs/why/runtime-boundary.md`
- `docs/why/toolchain-split.md`
- `docs/where/workspace-map.md`
