# SEAM VM Reference

SEAM is a typed runtime IR executed by a stack-based virtual machine. It is not generic machine assembly and it is not a byte-for-byte lowering of Musi surface syntax.

The clean-room SEAM layer is split deliberately:

- `music_il` defines the runtime contract
- `music_assembly` moves that contract between binary and text forms

The VM that eventually executes SEAM is downstream of that boundary. The SEAM docs define what the machine must honor, not which crate currently happens to execute it.

## Identity

| Property            | Value                                   |
| ------------------- | --------------------------------------- |
| Execution model     | stack machine                           |
| IR level            | typed runtime IR                        |
| Binary artifact     | `.seam`                                 |
| Human-readable form | SEAM text IL                            |
| Contract crates     | `crates/music_il`, `music_assembly` |

SEAM stays small by lowering source sugar away. It stays expressive by keeping explicit operations for the runtime concepts the VM actually owns.

## Runtime Value Model

SEAM uses uniform runtime values plus metadata tables.

- scalars may use NaN-boxed inline representations
- heap values remain explicit runtime objects
- the artifact carries string, type, effect, class, foreign, method, and global metadata
- the VM executes against decoded program metadata, not source syntax

The VM directly understands runtime object categories such as:

- closures
- continuations
- sequences
- aggregates
- strings
- C pointers
- mutable cells

That is the right abstraction level for an embeddable language runtime. It keeps host interaction and inspection explicit without leaking source-language sugar into the machine.

## Execution Model

### Entry And Startup

The language-level startup model remains top-level module execution.

- source entry selection comes from CLI or `musi.json`
- `index.ms` remains the default lookup when the manifest does not override it
- lowering synthesizes module entry methods
- initialization runs module top levels once before export use

There is no source-language `main` requirement and no `@main` attribute in the language model.

### Frames, Stack, And Globals

The VM owns:

- a call-frame stack
- an operand stack inside the active frame
- initialized globals
- resolved constant values
- active effect handlers

This stays a classic stack VM. Performance work belongs in compact operands, dispatch, value representation, and object layout, not in turning SEAM into register assembly.

## VM-Level Semantic Families

SEAM keeps explicit operations only for runtime concepts the VM genuinely owns.

### Calls And Closures

- direct call
- tail call
- return
- closure allocation
- local/global/upvalue access

### Sequences

The sequence family is explicit because the VM and GC care about sequence objects directly.

- allocation
- indexed access and mutation
- length
- slicing
- fill/copy/concatenation

### Aggregates

ADTs and record-like aggregates are their own family.

- construction
- field projection
- field mutation
- variant-tag inspection

This keeps algebraic data explicit without pretending they are just tagged sequences.

### Runtime Type Operations

Runtime typing remains explicit:

- type check
- type cast
- runtime type-id inspection

The VM does not conflate aggregate variant tags with runtime type identity.

### Resumable Algebraic Effects

Effects stay explicit in SEAM.

- handler push
- handler pop
- effect invocation
- continuation resumption

Resumable algebraic effects are a real VM concern, not emitter sugar over ordinary calls.

### Foreign Boundary

Foreign calls remain explicit VM operations coordinated with runtime metadata and the host boundary.

- foreign descriptors live in `.seam`
- the host/runtime boundary resolves libraries and symbols
- `ffi.call` is the public machine contract

GC pinning is not a public opcode. If the runtime needs pinning for foreign calls, that stays behind the `ffi.call` implementation.

## Garbage Collection

SEAM targets a mark-region heap.

- allocation and object layout are VM concerns
- safepoints exist at runtime-relevant operations such as calls, allocation, foreign calls, and effect transitions
- stacks, frames, globals, constants, continuations, and handler state all contribute to root discovery

The important design rule is:

- GC sees runtime object families
- GC does not need source-level constructs to survive into bytecode

## Embeddability

SEAM is designed to embed cleanly.

- hosts load a program artifact
- hosts build a VM
- hosts initialize module top levels
- hosts invoke exports explicitly
- hosts inspect results through stable value views

The stable embedding API sits above raw bytecode details, but the bytecode contract stays explicit enough that loaders, VMs, host integration, and docs agree on the same machine.

## Design Boundaries

SEAM does not own:

- piecewise conditionals
- comprehensions
- derivation
- matrix literals
- range syntax
- optional/result sugar
- built-in list cons
- typeclass dispatch as a VM primitive

SEAM does own:

- values
- control transfer
- closures
- sequences
- aggregates
- runtime typing
- resumable effects
- foreign calls
