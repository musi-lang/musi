# SEAM Lowering Contract

Status: proposed

This spec defines what “lowered enough for SEAM” means.

Musi and any other frontend target SEAM by emitting explicit runtime machinery, not by preserving source-language abstractions.

## Principle

SEAM receives lowered, manual boilerplate.

That means the frontend must perform the abstraction break before `.seam` or `.seamil` emission.

SEAM is not where source-language constructs stay pretty.

## Rust Comparison

Rust 2024 is implementation substrate after or beside SEAM lowering. Rust MIR/LLVM/Cranelift concepts may inform backend implementation, but SEAM is not Rust IR and must not inherit Rust syntax or trait terminology.

Frontend lowering may use Rust-style implementation strategies internally: explicit environment structs, vtables, match lowering, or unsafe wrappers. The emitted SEAM contract remains Musi/SEAM terms: contextual arguments, handler frames, continuations, native pointers, and verified domains.

## What Must Lower Away

These source-level ideas must not survive into SEAM as direct VM concepts:

- pattern matching
- source `given` contextual visibility
- range syntax
- source effect / answer / handle syntax
- source import syntax
- source record update sugar
- source destructuring sugar
- source borrow/pin syntax

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
- explicit handler-frame setup and unwind paths
- explicit contextual value arguments and dispatch calls
- explicit native pointer and pin-region operations

## Lowering Responsibilities By Concept

### Pattern Matching

Pattern matching lowers to:

- tag reads or type tests
- branch ladders or jump tables
- explicit field extraction
- explicit fallback paths

SEAM has no `match`, `tag`, `unwrap`, sum, option, or result opcode. Variant tags are verifier-approved layout fields read with `ld.fld`.

### Closures

Closures lower to:

- environment allocation
- explicit capture packing
- explicit callee and environment pairing
- explicit indirect call path

Function values use `ld.fn`, `new.fn`, and `call.ind`. Virtual, interface/shape, dynamic message, and foreign ABI calls use `call.virt`, `call.iface`, `call.dyn`, and `call.ffi`.

### Contextual Values

`given` contextual values lower to:

- dictionary values
- explicit dictionary passing
- explicit member dispatch

SEAM does not treat source-level `given` modifiers as VM objects.

Dictionary dispatch uses ordinary object and stack-call operations such as `ld.fld` and `call.ind`. VM-assisted shape/protocol dispatch uses `call.iface` when the descriptor declares an interface/shape member.

### Ranges

Ranges lower to:

- ordinary data layouts
- helper calls
- explicit comparison and stepping paths

SEAM does not require range-specific VM primitives.

Tuples, records, variants, options, and results lower to `new.obj`, `ld.fld`, `st.fld`, and `ld.fld.a` over layout descriptors. There are no tuple, sum, option, result, or range opcodes.

### Views And Pinning

Borrow-like views lower to `managed.views` machinery, and stable-address regions lower to `native.pin` machinery.

Address-like stack operations are `ld.fld.a`, `ld.elem.a`, `ld.ind`, `st.ind`, `pin`, `unpin`, and `ld.addr`. There is no arbitrary pointer arithmetic opcode.

### Effects, Answers, And Handling

Source `effect` / `answer` / `handle` forms lower to `resumable` machinery:

- handler frame push and pop through `hdl.push` and `hdl.pop`
- operation invocation through `raise`
- one-shot continuation resume through `resume`
- unwind restoration

`raise` captures the continuation according to the operation descriptor. Handler clauses receive the continuation as an ordinary parameter when their descriptor needs resumption. There is no `ask`, `answer`, or `handler` opcode.

### Arithmetic

Source languages choose numeric opcodes that match their safety contract.

- `add`, `sub`, and `mul` are wrapping/modulo for fixed-width integers and IEEE for floats.
- `add.ovf`, `sub.ovf`, and `mul.ovf` trap on integer overflow.
- signed/unsigned operations use `.s` and `.u`.
- Musi source-level checked arithmetic lowers to `.ovf` opcodes.

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

- `specs/seam/format.md`
- `specs/seam/domains.md`
- `specs/seam/bytecode.md`
