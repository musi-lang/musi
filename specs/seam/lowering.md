# SEAM Lowering Contract

Status: frozen 0.1.0 baseline (2026-05-14)

This spec defines what “lowered enough for SEAM” means.

Musi and any other frontend target SEAM by emitting explicit runtime machinery, not by preserving source-language abstractions.

## Principle

SEAM receives lowered, manual boilerplate.

That means the frontend must perform the abstraction break before `.seam` emission.

SEAM is not where source-language constructs stay pretty.

## Rust Comparison

Rust 2024 is implementation substrate after or beside SEAM lowering. Rust MIR/LLVM/Cranelift concepts may inform backend implementation, but SEAM is not Rust IR and must not inherit Rust syntax or trait terminology.

Frontend lowering may use Rust-style implementation strategies internally: explicit environment structs, vtables, match lowering, or unsafe wrappers. The emitted SEAM contract remains Musi/SEAM terms: contextual arguments, native runtime calls, native pointers, and verified domains.

## What Must Lower Away

These source-level ideas must not survive into SEAM as direct VM concepts:

- pattern matching
- source context visibility
- range syntax
- source suspension syntax
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
- explicit contextual value arguments and dispatch calls
- explicit native runtime module calls

## Lowering Responsibilities By Concept

### Expression And Module Shape

Top-level statements lower as expression results whose value is discarded after
the mandatory semicolon. Final expressions lower to the surrounding procedure or
block result.

Modules lower as record-shaped values plus artifact import/export metadata.
Static package imports are resolved before SEAM emission. Dynamic module records
use action-first `ld.mod.dyn` and `ld.exp.dyn` when runtime lookup is required.

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

Function values use `new.fn` and `call.ind`. Foreign ABI calls use `call.ffi`.

### Contextual Values

Context values lower to:

- dictionary values
- explicit dictionary passing
- explicit member dispatch

SEAM does not treat context markers as VM objects.

Dictionary dispatch uses ordinary object and stack-call operations such as `ld.fld` and `call.ind`.

### Ranges

Ranges lower to:

- ordinary data layouts
- helper calls
- explicit comparison and stepping paths

SEAM does not require range-specific VM primitives.

`in` over a range lowers to explicit boundary comparisons. `in` over a
collection lowers to the collection protocol helper selected by the source type.

Tuples, records, variants, options, and results lower to `new.obj`, `ld.fld`, and `st.fld` over layout indexes. There are no tuple, sum, option, result, or range opcodes.

### Fallback

`??` lowers through the public Maybe/Expect library contract. The emitted code
performs the same tag test and branch structure used for ordinary variants,
then loads the contained value or evaluates the fallback expression.

### Spreads And Destructuring

Record spread lowers to explicit field copy and override construction. Array
spread lowers to sequence construction helpers when runtime length participates
in the result. Pattern destructuring lowers to explicit field or element loads,
tests, branches, and local stores.

### Views And Pinning

Borrow-like views and stable-address regions lower through native runtime modules.

SEAM has no address-like stack opcodes and no arbitrary pointer arithmetic opcode.

`pin` lowers to a native-domain pin lease helper with an explicit lexical
release point. The verifier contract is domain based: pinned managed addresses
may cross native calls only inside the active pin region.

### Unsafe

`unsafe` is a source permission boundary. It lowers by allowing operations whose
descriptors require native-domain permission, such as foreign calls and pin
leases. The emitted instructions stay ordinary `call.ffi`, `ld.ffi`, object, and
call operations with verifier-visible descriptors and domain requirements.

### Suspension And Drivers

Suspending operations lower to native runtime machinery:

- host behavior through native calls and module loading
- operation invocation through `call.ffi` or module export calls
- explicit state values when a runtime protocol needs resumption

`yield` lowers to a runtime suspension protocol: package/frontend code emits an
explicit state value and calls the selected driver helper. Resume points are
ordinary blocks and locals after lowering.

### Defer

`defer` lowers to explicit cleanup calls scheduled at every expression-block
exit path. Cleanup ordering is part of frontend lowering and must be visible as
ordinary calls and branches in SEAM.

### Known And Syntax

`known` is Musi's compile-time evaluation boundary, analogous to Zig `comptime`
and C++ `constexpr` / `consteval`. Lowering evaluates the marked expression
before runtime SEAM emission when required, then emits the resulting constant,
syntax value, generated module, or ordinary runtime value. Syntax templates
lower through syntax constants and `musi:syntax` host services.

### Templates And Runes

String templates lower to string constants and concatenation/format helpers.
Rune literals lower to the canonical rune runtime value representation through
constants or helper construction.

### Arithmetic

Source languages choose numeric opcodes that match their safety contract.

- `add`, `sub`, and `mul` are core arithmetic operations.
- `div.s` and `rem.s` are signed integer operations.
- Musi source-level checked arithmetic lowers through helper calls.

## Public Targeting Rule

Third-party frontends may target `.seam` directly if they obey this lowering contract.

That means a frontend does not need to mimic Musi syntax, but it must emit code that is already lowered to SEAM’s runtime contract.

## Text And Binary Equivalence

Textual bytecode is not a richer target than `.seam`.

It is the readable `disasm`/assembler view of the same lowered contract.

If something cannot round-trip to `.seam`, it is not part of SEAM’s public target contract.

## Backend Boundary

Backend lowering consumes verified SEAM modules after verification and decoding.

## See Also

- `specs/seam/format.md`
- `specs/seam/domains.md`
- `specs/seam/bytecode.md`
