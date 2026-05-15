# SEAM Design Documents — Index and Principles

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: frozen 0.1.0 baseline active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Status: normative freeze document (0.1.0 baseline).

Roadmap linkage: `docs/__smallcore__/PLAN.md`

This set of documents defines the SEAM bytecode and VM direction for the stripped-down Musi language. It assumes the source language has already frozen the small-core direction: expression-based source, `if then else`, `match`, `let ... else`, `mut`, `known`, `pin`, `defer`, `yield`, `hidden`, `erased`, `Maybe`, `Expect`, `?T`, `E!T`, `??`, capability objects, UFCS/UDNS, and source maps as the only authorship recovery layer.

SEAM means **Stack Effect Abstract Machine**. The name must be literal, not decorative.

```text
Stack Effect  every instruction, block, call, branch, yield point, and return has an exact stack transition.
Abstract      SEAM is not Musi source, not Rust MIR, not LLVM IR, not Java bytecode, and not CPU assembly.
Machine       SEAM is an executable module image with bytecode, tables, roots, frames, heap, domains, and loader rules.
```

The canonical compiled module artifact is:

```text
.seam
```

The package/archive artifact is:

```text
.mar
```

A `.mar` is analogous to a `.jar`: a Musi archive. Build profiles determine how much source/debug/authorship information survives. A `.mar` can be debug-rich, source-retaining, release-stripped, thin, fat, flattened, and/or obfuscated depending on profile. The archive format itself is stable; minification/obfuscation/source retention are profile decisions.

There is no canonical `.seamil` peer file. Textual bytecode is a **view** of `.seam`, produced by `disasm`, not a second public artifact.

## Document set

1. `seam-00-index-and-principles.md` — this file; identity, principles, historical lessons, and freeze rules.
2. `seam-01-bytecode-and-stack-effects.md` — bytecode artifact shape, opcode/mnemonic design, stack-effect notation, verifier model, and branch rules.
3. `seam-02-calls-objects-and-layouts.md` — function/call/frame model plus object, variant, data, layout, module, and descriptor model.
4. `seam-03-runtime-gc-pinning-yield-defer.md` — runtime, generational mark-region/Immix GC, root maps, `defer`, `pin`, `yield`, and coroutine frame design.
5. `seam-04-external-artifacts-decomp-mar.md` — external ABI boundary, `@foreign`, `.seam`, `.mar`, debug/release/fat/thin profiles, `disasm`, `decomp`, source maps, and name mangling.

Together these cover the nine SEAM design topics:

```text
1. SEAM bytecode artifact shape
2. stack-effect verifier model
3. function/call/frame model
4. object/variant/layout model
5. GC root maps and Immix/mark-region runtime
6. defer/pin/yield lowering
7. coroutine frame representation
8. external boundary and ABI contracts
9. decompiler/minifier/source-map format
```

## Core thesis

SEAM is not “Musi source encoded.” It is a compact, verified machine image. Musi is one frontend that lowers into SEAM. A `.seam` artifact stores executable semantics, not authored source.

```text
Musi source
  -> lowered Musi / compiler IR
  -> verified SEAM module
  -> loader-specialized runtime form
```

A decompiler without maps emits canonical lowered Musi: valid Musi, but normalized, expanded, renamed, stripped of original source shape, and possibly compacted. It is analogous to TypeScript compiling to raw JavaScript or Ghidra/IDA producing reconstructed pseudocode: behavior remains inspectable, authorship does not.

With source maps, tooling can project back toward authored source. Without maps, decompilation uses generated names and lowered forms.

## Mistake ledger from existing bytecodes

SEAM should learn from other VM systems without cloning them.

### JVM / `.class`

Take:

```text
verified module artifacts
operand-stack discipline
constant-pool style indirection
explicit stack transitions in instruction specification
```

Avoid:

```text
type-specialized public opcode explosion
legacy source-language object model dominating the artifact
too many baked-in historical conveniences
```

SEAM should keep verification and stack effects, but avoid multiplying public opcodes by every primitive type unless it materially improves compactness or dispatch.

### CIL / CLI

Take:

```text
metadata as a real artifact component
multi-language targeting discipline
clear separation between instruction set, metadata, debug information, and libraries
```

Avoid:

```text
heavy class/interface worldview as VM identity
metadata bloat beyond what the VM and loader need
source-public API shape dominating runtime shape
```

SEAM should use compact type/layout/function/domain descriptors, not a universal OO metadata system.

### Lua bytecode

Take:

```text
small VM footprint
compact instruction encoding
interpreter-friendly bodies
```

Avoid:

```text
bytecode too private to a runtime version if `.seam` is meant to be stable
weak validation story
source/runtime version fragility
```

SEAM may be compact like Lua, but its public artifact must be specified and verified.

### BEAM / `.beam`

Take:

```text
stable public module artifact
load-time rewriting and specialization
runtime-private internal opcodes distinct from public bytecode
```

SEAM should have stable generic public opcodes, then allow the loader to specialize into runtime-private internal opcodes, threaded dispatch forms, quickened forms, fused kernels, or JIT forms.

### WebAssembly

Take:

```text
validation-first stack discipline
structured, fast-checkable control/stack behavior
small binary target
```

Avoid:

```text
under-describing Musi-specific GC/layout/domain needs
forcing every higher-level descriptor into a minimal wasm-like shape
```

### LLVM IR

Take:

```text
separate text and binary views
strong verifier discipline
clear low-level semantics
```

Avoid:

```text
making public SEAM an optimizer IR
SSA as the executable artifact model
internal optimizer instability leaking into `.seam`
```

SEAM is a VM bytecode artifact, not a compiler optimizer IR.

## Display text vs binary identity

Mnemonic text is a display layer. Dotted text like:

```text
ld.loc
call.ffi
new.obj
cmp.eq
```

is not the bytecode identity. Binary `.seam` stores numeric opcode ids, variant ids, descriptor indexes, immediates, and table offsets.

A disassembler shows canonical dotted mnemonics:

```text
ld.loc 0
call.ffi 3
```

The text style is fixed for this baseline.

## Opcode naming constraints

Mnemonic segments should be short, conventional, and technological. Segment length should usually be 2–5 characters.

Frozen root families:

```text
ld     load/push
st     store/pop
new    construct/allocate
br     branch
call   call
ret    return
cmp    compare
drop   discard
dup    duplicate
swap   swap
cast   cast
is     runtime test
trap   trap/abort
pin    pin lease op, if represented in bytecode
yld    yield/suspend op, if represented in bytecode
```

Frozen branch and module qualifiers:

```text
br.z        branch when top Bit = 0
br.tbl      branch-table dispatch
ld.mod.dyn  dynamic module load
ld.exp.dyn  dynamic module export lookup
```

Frozen target/operand qualifiers:

```text
loc    local
glob   global
fld    field
elem   element
len    length
c      constant pool
i4     Int32 compact immediate
obj    object/layout aggregate
arr    array
fn     function/callable
ind    indirect target
ffi    foreign ABI edge
tail   tail call qualifier
type   type descriptor
mod    module qualifier
exp    export qualifier
dyn    dynamic lookup qualifier
tbl    branch-table qualifier
```

No aliases. No cute names. No source words as opcodes unless the VM transition is truly identical.

Rejected opcode names:

```text
match
if
let
known
hidden
erased
shape
maybe
expect
tuple
sum
pipeline
letElse
```

Those are source or lowering concepts, not SEAM primitive stack transitions.

## Public vs internal opcodes

SEAM must distinguish:

```text
public generic opcodes
  serialized in `.seam`
  stable across compatible runtime versions
  verified by public rules

loader/internal opcodes
  generated after `.seam` load
  runtime-private
  may be specialized by type/layout/domain/runtime mode
  not serialized as public `.seam`
```

Example:

```text
public:   ld.fld fieldId
internal: ld.fld.ref.off8
internal: ld.fld.word.off16
internal: ld.fld.inline.tag
```

The public opcode remains stable. The runtime may specialize aggressively.

## Source lowering rule

Musi source conveniences do not survive directly into SEAM.

Source-level constructs that must lower away:

```text
pattern matching
if/then/else source shape
let-else
pipeline |>
UFCS/UDNS method shape
Maybe/Expect sugar
?? fallback
source import syntax
source pin syntax
source defer syntax
source yield syntax
source mut sugar where it becomes slot/layout rules
```

SEAM receives explicit machinery:

```text
locals
blocks
branches
jump tables
layout-guided field loads/stores
object construction
function values
closure environments
runtime helper calls
foreign calls
pin lease descriptors
root maps
domain descriptors
```

## Artifact philosophy

`.seam` is the canonical executable module.

`.mar` is the canonical archive/package format.

`disasm` displays bytecode.

`decomp` displays canonical lowered Musi.

A source map is the only authorship recovery layer.

```text
without map: preserve semantics, public/API/link names, and built-ins only
with map: recover source names, spans, authored structure, comments if stored, and source sugar projection
```

## Freeze rules

Before numeric opcode positions freeze, the following must be frozen first:

```text
1. text mnemonic grammar and segment rules
2. public/internal opcode split
3. stack-effect notation and rightmost-is-top convention
4. block signature and branch target rules
5. method/function result-stack rules
6. type/layout/constant/function/import/export table responsibilities
7. root-map format contract
8. defer/pin/yield lowering contract
9. `.seam`/`.mar`/map/decomp/disasm artifact policy
```

Numeric opcode values should be the last thing to freeze.

## Current repo alignment references

These design documents align with the current repo direction where possible, but intentionally revise unsettled pieces:

```text
docs/where/bootstrap-bytecode-ledger.md
specs/seam/bytecode.md
specs/seam/format.md
specs/seam/lowering.md
specs/seam/domains.md
specs/runtime/memory-model.md
specs/runtime/interop.md
docs/reference/performance.md
docs/reference/builtins.md
```

The main deliberate revisions are:

```text
`.seamil` becomes a disassembly view, not a canonical peer artifact.
Dotted mnemonic spelling becomes display text, not bytecode identity.
Old module-first spellings are removed; canonical forms are `ld.mod.dyn` and `ld.exp.dyn`.
`.mar` is a stable archive format with debug/release and thin/fat profiles.
```
