# SEAM Lowering Contract

Status: proposed

This spec defines what “lowered enough for SEAM” means.

Musi and any other frontend target SEAM by emitting explicit runtime machinery, not by preserving source-language abstractions.

## Principle

SEAM receives lowered, manual boilerplate.

That means the frontend must perform the abstraction break before `.seam` or `.seamil` emission.

SEAM is not where source-language constructs stay pretty.

## What Must Lower Away

These source-level ideas must not survive into SEAM as direct VM concepts:

- pattern matching
- class and instance declarations
- range syntax
- borrow syntax
- pin syntax
- source effect syntax
- source import syntax
- source record update sugar
- source destructuring sugar

## What SEAM Receives

SEAM receives explicit machinery such as:

- locals
- blocks
- labels
- branches
- jump tables
- layout-guided field loads and stores
- explicit closure environment values
- explicit continuation payloads
- explicit handler frame setup and unwind paths
- explicit dictionary values and dispatch calls
- explicit native pointer and pin-region operations

## Lowering Responsibilities By Concept

### Pattern Matching

Pattern matching lowers to:

- tag reads or type tests
- branch ladders or jump tables
- explicit field extraction
- explicit fallback paths

### Closures

Closures lower to:

- environment allocation
- explicit capture packing
- explicit callee and environment pairing
- explicit indirect call path

### Classes And Instances

Classes and instances lower to:

- dictionary values
- explicit dictionary passing
- explicit member dispatch

SEAM does not treat source-level classes as first-class language objects.

### Ranges

Ranges lower to:

- ordinary data layouts
- helper calls
- explicit comparison and stepping paths

SEAM does not require range-specific VM primitives.

### Borrows, Slices, And Pinning

Borrow and view concepts lower to `managed.views` and `native.pin` machinery:

- `Ref[T]` to verifier-tracked ephemeral `ref`
- `MutRef[T]` to verifier-tracked ephemeral `mutref`
- `Slice[T]` to verifier-tracked ephemeral `slice`
- `pin` to explicit pin-region operations

### Effects And Handlers

Source effect syntax lowers to `resumable` machinery:

- handler frame push and pop
- continuation capture
- resume edges
- unwind restoration

## Public Targeting Rule

Third-party frontends may target `.seam` or `.seamil` directly if they obey this lowering contract.

That means a frontend does not need to mimic Musi syntax, but it must emit code that is already lowered to SEAM’s runtime contract.

## Text And Binary Equivalence

`.seamil` is not a richer target than `.seam`.

It is the readable textual twin of the same lowered contract.

If something cannot round-trip to `.seam`, it is not part of SEAM’s public target contract.

## JIT Boundary

If a JIT exists, it consumes verified SEAM modules.

Cranelift lowering happens after SEAM verification and decoding. Cranelift IR is internal backend machinery, not a public targeting format.

## See Also

- [seam-format.md](/Users/krystian/CodeProjects/musi-next/specs/seam-format.md)
- [seam-domains.md](/Users/krystian/CodeProjects/musi-next/specs/seam-domains.md)
