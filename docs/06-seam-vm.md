# SEAM VM Reference

SEAM is a typed runtime IR executed by a stack-based virtual machine. It is not generic machine assembly and it is not a byte-for-byte lowering of Musi surface syntax. The VM owns the runtime contracts that matter for execution, embedding, foreign calls, effects, aggregate storage, and garbage collection.

## Identity

| Property            | Value                                       |
| ------------------- | ------------------------------------------- |
| Execution model     | stack machine                               |
| IR level            | typed runtime IR                            |
| Binary artifact     | `.seam`                                     |
| Human-readable form | SEAM text IL                                |
| Embedding boundary  | `musi_vm::{load, Program, Vm, RuntimeHost}` |

SEAM stays small by lowering source-level sugar away. It stays expressive by keeping explicit VM-level operations for runtime concerns that do not collapse cleanly into plain loads, stores, calls, and branches.

## Runtime Value Model

SEAM uses uniform runtime values plus metadata tables.

- scalars use NaN-boxed inline representations where possible
- heap values remain explicit VM objects
- the binary format carries string, type, effect, class, foreign, method, and global metadata
- the VM executes against decoded `Program` metadata, not against source syntax

The VM directly understands these runtime object categories:

- closures
- continuations
- arrays
- slices
- strings
- C pointers
- cells

That is the right abstraction level for an embeddable language runtime. It keeps host interaction, GC, and inspection explicit without leaking source-language sugar into the machine.

## Execution Model

### Entry And Startup

`musi run` executes the selected module’s top level.

- source entry file selection comes from CLI or `musi.json`
- `index.ms` remains the default module lookup when the manifest does not override it
- the emitter synthesizes module entry methods
- `Vm::initialize()` runs entry methods once
- `Vm::run()` is the convenience wrapper over initialization

There is no source-language `main` requirement and no `@main` attribute in the language model.

### Frames, Stack, And Globals

The VM owns:

- a call-frame stack
- an operand stack inside the active frame
- initialized globals
- resolved constant values
- active effect handlers

This stays a classic stack VM. Performance work belongs in instruction dispatch, compact operands, value representation, and object layout, not in turning SEAM into register assembly.

## VM-Level Semantic Families

SEAM keeps explicit operations for runtime concepts that the VM genuinely owns.

### Calls And Closures

- direct call
- tail call
- return
- closure allocation
- upvalue/global/local access

### Aggregates

The aggregate family remains explicit because the VM and GC care about these objects directly.

- allocation
- indexed load/store
- slicing
- fill/copy/concatenation
- tagged aggregate support

The current family uses `arr.*` mnemonics. That is the sequence/aggregate family in the VM contract, even if future naming becomes broader.

### Runtime Type Operations

Runtime typing remains explicit:

- type check
- type cast
- runtime tag access

This keeps casts, gradual/runtime checks, host inspection, and aggregate tagging honest at the VM boundary.

### Resumable Algebraic Effects

Effects stay explicit in SEAM.

- handler push
- handler pop
- effect invocation
- continuation resumption

Resumable algebraic effects are a real VM concern, not just emitter sugar over ordinary calls.

### Typeclass Dictionary / Evidence Dispatch

Typeclass dispatch stays explicit in SEAM.

- dictionary/evidence acquisition
- dictionary method invocation

This keeps generic class-driven execution visible in the runtime contract instead of hiding it inside ad hoc call conventions.

### Foreign Boundary

Foreign calls remain explicit VM operations coordinated with runtime metadata and the host boundary.

- foreign descriptors live in `.seam`
- `RuntimeHost` resolves libraries and symbols
- the VM owns marshaling control at the bytecode boundary

### GC Control

The VM also owns runtime-only coordination instructions such as pinning where the foreign boundary requires it.

## Garbage Collection

SEAM targets a mark-region heap.

- allocation and object layout are VM concerns
- safepoints exist at runtime-relevant operations such as calls, allocation, foreign calls, and effect transitions
- stack, frames, globals, constants, continuations, and handler state are all part of root discovery

The important design rule is:

- GC sees runtime object families
- GC does not need source-level constructs to survive into bytecode

## Embeddability

SEAM is designed to embed cleanly.

- hosts load a `Program`
- hosts build a `Vm`
- hosts initialize the module top level
- hosts invoke exports or values explicitly
- hosts inspect results through `ValueView`

The stable embedding API sits above raw bytecode details, but the bytecode contract stays explicit enough that loader, VM, host integration, and docs all agree on the same runtime machine.

## Design Boundaries

SEAM does not own:

- piecewise conditionals
- comprehensions
- derivation
- matrix literals
- range syntax
- optional/result sugar
- list-cons as a built-in concept

Those are either removed from the language core or lowered away before bytecode emission.

SEAM does own:

- values
- control transfer
- closures
- aggregates
- runtime typing
- resumable effects
- typeclass evidence dispatch
- foreign calls
- GC coordination
