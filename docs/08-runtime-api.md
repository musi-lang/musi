# Runtime API

This document defines the intended embedding boundary for `.seam` programs.

`crates/musi_vm` now provides the initial executable runtime crate, and this document defines its intended stable embedding boundary.

The runtime surface has two co-equal roles:

- `Program` as the loaded metadata view of a `.seam` artifact
- `Vm` as the execution engine over a loaded program

The Rust crate can be reorganized later. The stable boundary is the public API, not the current internal file layout.

## Runtime Roles

### `Program`

`Program` is the opaque loaded form of a `.seam` module or merged program.

It is not a raw decoded-module API. Its public job is read-only metadata inspection:

- string table access
- type descriptors
- effect descriptors
- class descriptors
- foreign descriptors
- canonical lookup helpers such as effect and effect-op lookup

Hosts may inspect program metadata without constructing a `Vm`.

### `Vm`

`Vm` is the runtime executor for a loaded `Program`.

It owns:

- globals
- heap
- call frames
- resolved constant values
- effect handlers
- initialization state

`Vm` is the boundary for execution, export lookup, dynamic module load, invocation, and runtime value inspection.

## Runtime Flow

### 1. Load

Loading decodes `.seam` bytes into a `Program`.

The result is opaque and validated. Embedding code interacts with it through stable metadata queries rather than raw loader structs.

### 2. Build A VM

A `Vm` is created from a loaded `Program`.

The default construction path should install a standard native host implementation; an alternative construction path should support a custom host boundary.

### 3. Initialize

Initialization is an explicit runtime step.

- merged module top-level entrypoints run exactly once
- those entrypoints are synthesized from module top-level execution
- there is no required source-language `main`
- there is no `@main`

Initialization is part of the observable embedding contract, not an internal detail.

Exports and ordinary invocation operate on an initialized runtime.

### 4. Execute, Load, And Inspect

After initialization, the runtime supports:

- export lookup by source name
- dynamic module load through the host boundary
- export lookup by loaded module handle
- invocation of a resolved callable value
- invocation of an exported callable by name
- invocation of an exported callable from one loaded module handle
- host-owned inspection of returned values

Stable value inspection belongs to view types, not raw heap/object internals.

## Host Boundary

The runtime host boundary is one host trait/object.

It owns:

- host effect handling
- runtime module loading for `mod.load`
- native library loading
- native symbol resolution
- syntax compile/eval for `Vm::eval_syntax`

This stays one embedding seam instead of splitting effects and native interop into separate host-capability systems.

The default native host is only one implementation of that boundary.

## Value Inspection

The stable embedding surface includes host-owned value views.

That inspection layer exists so embedding code can:

- inspect scalars directly
- inspect arrays and other runtime values through stable views
- avoid depending on heap layout or raw VM object structs

Inspection is read-only. Raw heap internals are not part of the intended embedding API.

## What The Runtime API Does Not Expose

The intended embedding surface does not make these stable:

- heap layout details
- raw decoded-module structs
- frame internals
- GC internals
- ad hoc runtime construction helpers used only by repo-local tests

Those remain implementation details or repo-internal helpers.

## Relationship To The Language Model

Runtime startup model targeted by the language and this contract is:

- the selected module’s top level is the program entry
- initialization runs those top levels once
- exported bindings become available after initialization

This is the runtime reflection of Musi’s top-level execution model, not a separate startup convention invented by the embedding layer.
