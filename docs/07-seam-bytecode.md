# SEAM Bytecode Reference

SEAM bytecode is the binary execution contract for the SEAM VM. It is paired with a first-class human-readable text IL used for docs, debugging, testing, and future assembly/disassembly tooling.

SEAM is:

- stack-based
- typed at the runtime-contract level
- organized by opcode family
- explicit about effects, typeclass dispatch, aggregates, FFI, and runtime typing

SEAM is not:

- generic machine assembly
- an AST dump
- split into “core” and “extension” opcode tiers

## Binary And Text Layers

There are three layers to keep distinct:

1. Musi source
2. SEAM text IL
3. `.seam` binary sections and encoded instructions

The text IL is the readable contract. The `.seam` binary is the execution artifact.

## Mnemonic Design

Mnemonics use lowercase dotted family names.

Examples:

- `ld.loc`
- `st.glob`
- `cmp.eq`
- `br.tbl`
- `call.tail`
- `arr.new`
- `ty.chk`
- `eff.invk`
- `tycl.dict`
- `ffi.call`

This naming scheme groups instructions by runtime family and scales better than flat names or source-level spellings.

## Text IL

SEAM text IL is directive-based and symbolic where possible.

### Design Rules

- directives describe metadata and declaration boundaries
- labels are symbolic
- locals use `%` slots
- globals, methods, types, effects, classes, and foreigns are named symbolically in text form
- numeric immediates remain numeric where they are truly immediate machine data

### Intended Shape

```text
.global answer export

.method @entry locals 1
L0:
  ld.smi 41
  st.loc %0
  ld.loc %0
  ld.one
  i.add
  ret
```

Additional directive families should cover:

- `.const`
- `.global`
- `.method`
- `.effect`
- `.class`
- `.foreign`
- optional debug directives later

This text form is meant to be readable first. The assembler/disassembler layer is responsible for lowering symbolic references to numeric indices used in the binary format.

## Operand Model

The binary instruction layer stays compact.

Current operand categories already fit the desired contract:

- none
- `u8`
- `u16`
- `i16`
- compact structured operands for closures, aggregates, effects, jumps, and tables

The text IL should present these as readable arguments rather than exposing raw operand enums directly.

Examples:

- `ld.loc %0`
- `ld.const @const:msg`
- `br.false L_else`
- `cls.new @fn:closure 2`
- `eff.invk @effect:Abort @op:abort`
- `tycl.call 0 2`

## Opcode-Space Policy

Opcode space is reserved by runtime family.

The family split should remain visible in both docs and code:

- data movement
- stack
- scalar arithmetic / bitwise / comparison
- branch
- call / closure
- aggregate / sequence
- runtime type operations
- effect / continuation
- typeclass dictionary / evidence
- FFI / host
- GC / runtime control

### Reservation Rule

Future additions should land in or adjacent to the relevant family band.

Do not treat the high unused range as a conceptual “extension set”. It is just unallocated opcode space. Growth should preserve family locality instead of scattering unrelated instructions into a generic extension pool.

### Current Reality

The current bytecode already has:

- family-local reserved holes
- a large unused tail range

The rewrite keeps the family-local story and treats the tail as capacity for future family expansion, not as a separate extension tier.

## Family-Level Semantics

### Data And Stack

These remain classic stack-machine instructions:

- loads
- stores
- stack shuffles
- constants

### Scalar Ops

Integer and float arithmetic stay explicit. Logical/bitwise operations stay unified where the VM genuinely executes the same class of operation.

### Calls And Closures

Call, tail call, return, and closure creation remain first-class instruction families. They are fundamental to the machine.

### Aggregates

The current family uses `arr.*`, but semantically it is the runtime aggregate/sequence family.

It covers:

- allocation
- indexing
- mutation
- length
- slicing
- fill/copy/concatenation
- tagged aggregate helpers

Collection-level source constructs stay lowered out of bytecode.

### Runtime Type Ops

Runtime checks and tags are explicit because the VM owns them directly.

### Effects

Resumable algebraic effects remain explicit SEAM instructions.

- handler push/pop
- effect invoke
- continuation resume

This is part of the VM contract, not an afterthought.

### Typeclass Dispatch

Dictionary/evidence dispatch remains explicit SEAM machinery.

The emitter may optimize or erase some static cases, but the runtime contract still includes a narrow explicit dispatch family for generic paths.

### FFI And GC

FFI and GC coordination stay explicit because they cross the host/runtime boundary and need stable machine-visible behavior.

## `.seam` Binary Sections

The binary format remains section-based:

- strings
- types
- constants
- dependencies
- globals
- methods
- effects
- classes
- foreign descriptors
- debug data

This section model is part of what makes SEAM typed and embeddable. The binary does not rely on hidden compiler-only conventions to recover runtime meaning.

## What SEAM Does Not Encode Directly

SEAM does not carry dedicated opcodes for removed or lowered-away source sugar.

That includes:

- piecewise conditionals
- comprehensions
- derivation
- matrix literals
- ranges
- optional/result sugar
- built-in list cons

If a source feature survives into the language, it must justify itself as a runtime concern before it becomes a bytecode concern.
