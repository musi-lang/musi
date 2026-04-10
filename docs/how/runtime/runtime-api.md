# Runtime API

**What**: operational guide to loading, initializing, executing, and inspecting SEAM programs.
**Why**: embedding should be easy to reason about without reading internal `musi_vm` code layout.
**How**: use this when integrating `musi_vm` into tools, hosts, tests, or external applications.
**Where**: public surface inventory lives in `docs/reference/public-api.md`; runtime model lives in `docs/what/runtime/seam-vm.md`.

## Core Runtime Roles

The embedding boundary has two co-equal pieces:

- `Program`: loaded `.seam` artifact view
- `Vm`: execution engine over one root program and loaded modules

Host behavior is supplied through `VmHost`.

## Load

Typical flow starts by materializing a program:

1. produce `.seam` bytes
2. call `Program::from_bytes`
3. inspect exports or metadata if needed

Use this stage for loader-level validation and artifact inspection before execution.

## Build And Initialize

Typical VM setup:

1. construct `Vm`
2. attach `VmHost` or use `NativeHost`
3. call `initialize`

Initialization runs synthesized entry/module-init logic exactly once per loaded module instance.

## Execute

Execution entrypoints are:

- `call_export`
- `call_module_export`
- `call_value`
- `load_module`

Use root-export calls for ordinary entrypoints and module-handle calls for dynamic module flow.

## Inspect

Inspection APIs expose stable runtime views:

- `Value`
- `ValueView`
- `SeqView`
- `RecordView`
- `StringView`

Use inspection for embedding and tests instead of reaching through VM internals.

## Host Integration

`VmHost` owns host-world seams:

- module loading policy
- foreign call behavior
- unhandled host effects
- syntax evaluation

Keep these policies in the host boundary rather than burying them inside the VM.

## Integration Checklist

When embedding:

- validate/load bytes first
- initialize before lookup/call
- keep host behavior explicit
- use stable value inspection APIs
- avoid depending on internal VM module layout

## See Also

- `docs/what/runtime/seam-vm.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/reference/public-api.md`
