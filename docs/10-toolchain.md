# Toolchain Reference

Musi uses two command surfaces with different audiences:

- `music`: the low-level compiler/runtime driver
- `musi`: the high-level package and operator tool

This document defines that split. It does not describe the current Rust crate layout.

## `music`

`music` is the direct language/runtime driver.

Its job is low-level work over source and `.seam` artifacts:

- check and analyze source
- build source into `.seam`
- run source or `.seam`
- inspect or disassemble emitted artifacts

`music` is the closest analogue to `rustc`, `javac`, `scalac`, or `tsc` plus a direct runtime driver for SEAM artifacts.

### `music` Flow

```text
source (.ms)
  -> analysis
  -> SEAM emission
  -> .seam
  -> runtime load / run / inspect
```

`music` works directly on files and artifacts. It is not the package manager.

## `musi`

`musi` is the package-aware operator tool.

Its job is project and package workflows:

- package-aware `run`
- `test`
- task execution
- dependency and package operations

`musi` resolves entries, package metadata, imports, and workspace-level operator concerns through `musi.json`.

## Manifest-Driven Operator Surface

`schemas/musi.schema.v1.json` already defines the operator/package shape that `musi` is expected to understand.

Important manifest-backed areas include:

- `main`
- `exports`
- `imports`
- `dependencies`
- `devDependencies`
- `peerDependencies`
- `optionalDependencies`
- `overrides`
- `tasks`
- `test`
- `workspace`

That is the package/operator contract for `musi`, even where the current binary surface is still smaller.

## Entry Selection

For package-aware `musi` workflows:

- the explicit CLI target wins when provided
- package configuration may define the entry
- otherwise package-style lookup falls back to `index.ms`

The language startup model stays top-level-driven:

- there is no required `main`
- there is no `@main`
- the selected module’s top level is the entry

## Command Tiers

### Core `musi` Operator Commands

The core high-level surface is:

- package-aware `run`
- `test`
- task execution
- dependency and package operations

These are the commands that make `musi` the operator tool rather than just another compiler frontend.

### Schema-Owned Tooling Areas

Some manifest sections describe tooling under the Musi framework without forcing those tools to be part of the compiler core itself.

These areas include:

- `fmt`
- `lint`
- `bench`
- `publish`
- `lock`
- `compile`

The important distinction is:

- they belong to the Musi tool ecosystem
- they are not automatically part of the low-level compiler/runtime driver

This is closer to Cargo’s umbrella-command model than to pretending everything must live inside the compiler binary itself.

## Why The Split Exists

The split keeps the toolchain honest:

- `music` owns direct language and SEAM work
- `musi` owns package and operator workflows
- schema-backed tooling can live under the Musi framework without being confused with compiler-core behavior

This gives Musi a cleaner shape than one giant binary that mixes compiler, runtime, package manager, and every auxiliary tool into one undifferentiated surface.

## Related Docs

For the machine and ownership boundaries behind this toolchain, read:

- `08-runtime-api.md`
- `09-architecture.md`
