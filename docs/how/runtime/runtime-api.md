# Runtime API

This page covers loading, initializing, executing, and inspecting SEAM programs.

## Core Runtime Roles

The runtime boundary has four main pieces:

- `Program`: loaded `.seam` artifact view
- `Vm`: execution engine over one root program and loaded modules
- `Runtime`: source-aware runtime service over `music_session` + `musi_vm`
- `NativeHost`: repo-owned host/world integration layer over `musi_vm`

Supporting that boundary, `musi_foundation` is the Rust-side owner of the `musi:*` module registry.

`VmHost` handles foreign calls and unhandled effects. `VmLoader` handles dynamic program loading. `Runtime` handles source-backed loading, syntax execution, and the repo-owned runtime path built on `musi_native`.

## Load

Typical flow starts by materializing a program:

1. produce `.seam` bytes
2. call `Program::from_bytes`
3. inspect exports or metadata if needed

Use this stage for validation and inspection before execution.

## Build And Initialize

Typical runtime setup:

1. construct `Runtime`
2. register module text or precompiled programs
3. construct `NativeHost`
4. call `load_root`

`Runtime::new(host, options)` takes an explicit `musi_native::NativeHost`.

Raw VM setup stays available for embedding-specific integrations:

1. construct `Vm`
2. attach `VmLoader`
3. attach `VmHost` or use `RejectingHost` + `RejectingLoader`
4. call `initialize`

Initialization runs synthesized entry/module-init logic exactly once per loaded module instance.

## Execute

Execution entrypoints are:

- `call_export`
- `call_module_export`
- `call_value`
- `load_module`
- `run_test_module`

Use root-export calls for normal entrypoints and module-handle calls for dynamic module flow.
Opaque exports remain visible in program metadata but are rejected by runtime export lookup/call APIs.

## Inspect

Inspection APIs expose stable runtime views:

- `Value`
- `ValueView`
- `SeqView`
- `RecordView`
- `StringView`

Use inspection instead of reaching through VM internals.

## Host Integration

`VmHost` handles:

- foreign call behavior
- unhandled host effects
- typed foreign/effect signature metadata through `ForeignCall` and `EffectCall`

`VmLoader` handles module source and program policy.

`Runtime` handles:

- source registration
- compile-on-demand module loading
- expression syntax evaluation with explicit result type
- module-syntax compilation and loading
- source-aware test-module execution over one explicit host boundary

`NativeHost` handles:

- registered foreign handlers
- registered effect handlers
- `musi:test` session collection and report output
- cfg-selected repo-owned platform dispatch for macOS/Linux/Windows
- explicit unsupported-target rejection
- embedding fallback delegation into one explicit `VmHost`

`musi:*` is the source-visible foundation namespace above the runtime and host boundary. `musi_foundation` is the Rust-side owner of that namespace's module registry. The foundation inventory is `musi:test` and `musi:syntax`.

## Integration Checklist

- validate/load bytes first
- initialize before lookup/call
- keep host/loader behavior explicit
- use stable value inspection APIs
- avoid depending on internal VM module layout

## See Also

- `docs/what/runtime/seam-vm.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/what/runtime/foundation-namespace.md`
- `docs/reference/public-api.md`
