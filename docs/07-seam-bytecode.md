# SEAM Bytecode Reference

SEAM bytecode is the binary execution contract for the clean-room SEAM layer in `crates/`.

- `music_bc` owns the SEAM ISA, metadata descriptors, and artifact model.
- `music_assembly` owns binary encoding/decoding, text transport, and validation.

SEAM is:

- stack-based
- typed at the runtime-contract level
- organized by opcode family
- explicit about sequences, aggregates, effects, FFI, and runtime typing

SEAM is not:

- generic machine assembly
- an AST dump
- split into “core” and “extension” opcode tiers
- a mirror of legacy crate structure

## Binary And Text Layers

There are three layers to keep distinct:

1. Musi source
2. SEAM text IL
3. `.seam` binary sections and encoded instructions

The text IL is the readable contract. The `.seam` binary is the execution artifact.

## Crate Boundaries

`music_bc` is the core domain crate.

- `isa/` owns opcodes, families, operands, and instructions
- `descriptors/` owns runtime-visible SEAM descriptors
- `artifact/` owns the single SEAM artifact model used by both encode and decode

`music_assembly` is the transport boundary crate.

- `binary/` owns binary encode/decode, section tags, versioning, and binary validation
- `text/` owns text IL formatting, parsing, assembly, and text validation

Each concept has one source of truth. There is no separate opcode count constant, no duplicated mnemonic table, and no legacy compatibility layout inside the clean-room crates.

## Mnemonic Design

Mnemonics use lowercase dotted family names.

Examples:

- `ld.loc`
- `st.glob`
- `cmp.eq`
- `br.tbl`
- `call.tail`
- `seq.new`
- `data.new`
- `ty.chk`
- `ty.id`
- `eff.invk`
- `ffi.call`

This keeps the ISA readable without encoding source-level sugar into bytecode names.

## Text IL

SEAM text IL is directive-based and symbolic where possible.

### Design Rules

- directives describe metadata and declaration boundaries
- labels are symbolic
- locals use `%` slots
- globals, methods, types, effects, and foreigns are symbolic in text form
- numeric immediates stay numeric only when they are truly machine data

### Intended Shape

```text
.global answer export

.method @entry locals 1
L0:
  ld.smi 41
  st.loc %0
  ld.loc %0
  ld.smi 1
  i.add
  ret
```

Additional directive families should cover:

- `.const`
- `.global`
- `.method`
- `.effect`
- `.foreign`

The text form is readable first. `music_assembly` lowers symbolic references to numeric indices in the binary format.

## Operand Model

The binary instruction layer stays compact.

Current operand categories are:

- none
- `u8`
- `u16`
- `i16`
- `wide(u16, u8)`
- `type_len(u16, u16)`
- `effect(u16, u16)`
- `effect_jump(u16, u16, i16)`
- branch tables

Examples:

- `ld.loc %0`
- `ld.const @const:msg`
- `br.false L_else`
- `cls.new @fn:closure 2`
- `seq.new @type:Bytes 64`
- `data.new @type:Option 1`
- `eff.invk @effect:Abort @op:abort`

`data.new` uses the tag value from the stack. The operand carries only the aggregate type id and field count.

## Opcode Families

The clean-room ISA keeps only semantic-core families:

- data movement
- stack
- scalar arithmetic
- logic
- compare
- branch
- call / closure
- `seq.*`
- `data.*`
- `ty.*`
- `eff.*`
- `ffi.*`

### Data And Stack

These remain classic stack-machine instructions:

- loads
- stores
- stack shuffles
- immediate scalar loads

### Scalar, Logic, And Compare

Integer and float arithmetic stay explicit. Logical and bit-oriented operators stay unified where the machine-level meaning is the same.

### Calls And Closures

Call, tail call, return, and closure allocation remain first-class instruction families.

### Sequences

`seq.*` is only for sequence-like runtime objects:

- allocation
- indexed access
- indexed mutation
- length
- slicing
- fill/copy/concatenation

Fixed-index fast paths are not part of the stable ISA.

### Aggregates

`data.*` is the first-class family for ADTs and record-like aggregates:

- construction
- field access
- field mutation
- variant-tag inspection

ADT tagging does not leak through `seq.*`.

### Runtime Type Operations

The runtime type family is explicit:

- `ty.chk`
- `ty.cast`
- `ty.id`

`ty.id` is runtime type-id inspection. Variant tags belong to `data.tag`, not `ty.*`.

### Effects

Resumable algebraic effects remain explicit SEAM instructions:

- handler push/pop
- effect invoke
- continuation resume

### Foreign Boundary

Foreign calls remain explicit VM operations coordinated through metadata and the host boundary.

Pinning is not a public opcode. If the runtime needs to pin values for FFI, that stays behind `ffi.call`.

## Removed From The Clean-Room ISA

These are intentionally absent from `crates_new/music_bc`:

- `ld.nil`
- `ld.one`
- `br.back`
- `seq.get.i`
- `seq.set.i`
- `seq.tag`
- `seq.new.tag`
- `ty.tag`
- `tycl.*`
- `gc.pin`
- `gc.unpin`
- `nop`
- `panic`
- `halt`

If a future profiling pass justifies new micro-ops, they belong in reserved family space, not in a generic extension tier.

## `.seam` Binary Sections

The binary format remains section-based:

- strings
- types
- constants
- globals
- methods
- effects
- classes
- foreign descriptors

This is enough metadata to keep the artifact typed and embeddable without encoding source syntax directly.

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
- typeclass dispatch as a VM primitive

If a source feature survives into the language, it only becomes a bytecode concern when it survives lowering as a durable runtime concept.
