# SEAM Bytecode and Stack-Effect Verifier

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: frozen 0.1.0 baseline active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Status: normative freeze document (0.1.0 baseline).

Covers:

```text
1. SEAM bytecode artifact shape
2. stack-effect verifier model
```

SEAM bytecode exists only for primitive VM transitions. A source feature does not get a SEAM opcode just because it is important in Musi source. If it can be lowered into locals, objects, branches, tables, calls, descriptors, and runtime helpers, it should lower before `.seam` emission.

## Bytecode artifact shape

A `.seam` file is one canonical SEAM module image. It is not source code and not a text IL file.

A `.seam` module contains at least:

```text
header
version
feature/domain manifest
constant table
type table
layout table
function table
import table
export table
foreign/external descriptor table
bytecode body table
block table / branch target metadata
stack-effect table
root-map table
optional debug/map reference
integrity/checksum/signature metadata
```

The exact binary section order can be optimized, but the conceptual tables must be stable.

### Header

The header should identify:

```text
magic bytes
SEAM bytecode version
minimum runtime version
endianness/encoding marker if needed
profile flags
section index offset
integrity flags
```

Recommended magic style:

```text
SEAM\0
```

or another fixed 4-byte magic. The exact bytes can be selected later, but the header must be cheap to recognize.

### Versioning

Use separate versions for:

```text
format version
bytecode instruction set version
domain feature version
metadata/table version if needed
```

Do not make the runtime version be the only compatibility number. Public `.seam` must be stable enough to load under compatible runtimes even when internal loader-specialized opcodes change.

### Constant table

The constant table stores literal/runtime constants:

```text
integers
words
bit patterns
strings
runes
layout-initializer constants
type tokens when needed
foreign symbol strings when not in external table
```

Constants should be deduplicated within one `.seam` module. `.mar` release/fat builds may deduplicate across modules.

### Type table

The type table stores canonical type descriptors required by the verifier and runtime.

Examples:

```text
Bit
Byte
Word
Word8
Word16
Word32
Word64
Int
Nat
Ptr[T]
Ref[T]
MutRef[T]
Slice[T]
Array[T]
Fn[params; results]
Obj layoutId
Erased shape descriptor
```

Source type spelling is not stored unless a map/debug layer asks for it.

### Layout table

The layout table stores object/variant/record layouts:

```text
layout id
kind: record | variant case | closure env | module record | runtime object
field count
field type ids
field offsets or logical indexes
tag info if variant-related
GC pointer bitmap
alignment and storage representation
visibility/export flags if needed
hidden/private flags
```

### Function table

The function table stores callable bodies and descriptors:

```text
function id
name id or generated name id
parameter/local type list
result stack type list
bytecode body pointer
block signature table pointer
root-map pointer
callability flags
export flags
domain requirements
```

### Import/export tables

Imports and exports are linking surfaces.

Import table:

```text
required module/package id
symbol/export id
type/function/layout expectation
domain requirements
resolution mode
```

Export table:

```text
export name
function/layout/data/module id
visibility shape
external exposure if applicable
```

### External descriptor table

External/ABI descriptors are separate from ordinary imports.

They should describe:

```text
external symbol or handle
ABI/calling convention descriptor
stack effect or ABI argument/result mapping
pin/lifetime requirements
nullable/raw pointer conventions
domain requirements
```

`@foreign` in source contributes to this table; direction follows shape (`body` presence plus `export`) per `seam-04-external-artifacts-decomp-mar.md`.

### Bytecode bodies

Bytecode bodies are compact sequences of:

```text
opcode id
variant or operand kind
immediate payload or descriptor index
```

Public serialized opcodes are generic. Loader-specialized internal forms are not serialized.

### Stack-effect table

Every opcode has a static transition. Every function/block has an exact incoming/result stack. The verifier uses these to ensure the module is well-typed.

### Root-map table

Each safe point maps locals/operand stack slots/captured state to root descriptors for the GC. Root maps are covered in `seam-03-runtime-gc-pinning-yield-defer.md`.

## Public opcode identity

The binary identity of an instruction is numeric. Text mnemonics are display strings only.

An instruction can be described conceptually as:

```text
opcodeFamily
opcodeVariant
operandKind
operandPayload
```

For example, textual:

```text
ld.loc 0
```

may represent:

```text
family  = ld
variant = loc
operand = u16(0)
```

The dot is display formatting, not a bytecode separator.

## Mnemonic segment convention

Text mnemonic segments should be 2–5 characters when practical.

### Core roots

```text
ld     load/push value
st     store/pop value
new    construct/allocate value
br     branch
call   call
ret    return
drop   discard stack value
dup    duplicate stack value
swap   swap top stack values
cmp    compare
cast   cast
is     runtime test
trap   trap/abort
pin    pin lease transition, if encoded
yld    yield/suspend transition, if encoded
```

### Variant/target segments

```text
loc    local
glob   global
fld    field
elem   element
len    length
c      constant table
i4     Int32 compact immediate
obj    object/layout aggregate
arr    array
fn     function/callable
ind    indirect target
ffi    foreign ABI edge
tail   tail call qualifier
type   type descriptor
mod    module
exp    export
dyn    dynamic lookup
z      branch-on-zero-Bit qualifier
tbl    branch table
```

### Rejected display naming

Reject source-shaped or verbose display names:

```text
loadLocal
storeField
branchIfZero
compareGreaterOrEqual
invokeExternalFunction
match
letElse
yieldStatement
```

Use canonical module mnemonics:

```text
ld.mod.dyn
ld.exp.dyn
```

## Public vs loader-specialized opcodes

Public `.seam` bytecode should avoid opcode explosion.

Public:

```text
ld.fld fieldId
st.fld fieldId
call methodId
cmp.eq
```

Loader-specialized internal forms may exist:

```text
ld.fld.ref.off8
ld.fld.word.off16
call.mono.inline
cmp.eq.word
```

Only public opcodes are stable and serialized. Internal opcodes are runtime-private.

This copies BEAM’s useful separation: public transport bytecode can stay generic while the loader rewrites into runtime-specific fast forms.

## Stack-effect notation

Stack effects use:

```text
[inputs ; outputs]
```

Rightmost input is top-of-stack.

Examples:

```text
[;]                    consumes nothing, produces nothing
[; T]                  pushes T
[T ;]                  pops T
[A ; A, A]             duplicates A
[A, B ; B, A]          swaps two top values
[Word, Word ; Word]    consumes two Words, produces one Word
[Bit ;]                consumes a Bit condition
[args... ; results...] consumes function args and produces result stack
```

Stack-effect notation appears in:

```text
spec tables
verifier diagnostics
external/bytecode descriptors
possibly source-level stack annotations at low-level boundaries
```

It is not a general runtime value.

## Type variables in stack effects

Stack effects may use metavariables:

```text
[A ;]
[A ; A, A]
[A, B ; B, A]
```

A verifier instantiates them per instruction occurrence.

Rules:

```text
A, B, C are stack type variables, not Musi type parameters.
N means numeric type constrained by opcode descriptor.
S means arbitrary stack prefix retained by a branch transfer.
results means the function's declared result stack.
```

## Example instruction effects

```text
ld.loc n        [; T]
st.loc n        [T ;]
ld.glob g       [; T]
st.glob g       [T ;]
ld.fld f        [Obj ; T]
st.fld f        [Obj, T ;]
ld.elem         [Array[T], Int ; T]
st.elem         [Array[mut T], Int, T ;]
ld.len          [Array[T] ; Nat]
ld.c c          [; T]
new.obj id,n    [field0, field1, ... fieldN ; Obj]
new.arr T,n     [item0, item1, ... itemN ; Array[T]]
new.fn m,c      [capture0, ... captureC ; Fn]
call m          [args... ; results...]
call.ind        [Fn, args... ; results...]
call.ffi d      [args... ; results...]
ret             [results... ;]
br target       [S ; target.S]
br.z target [S, Bit ; target.S]
br.tbl table    [S, Int ; selectedTarget.S]
drop            [A ;]
dup             [A ; A, A]
swap            [A, B ; B, A]
cmp.eq          [A, A ; Bit]
cmp.ne          [A, A ; Bit]
cmp.lt          [A, A ; Bit]
add             [N, N ; N]
and             [A, A ; A]
not             [A ; A]
trap            [; Never] or polymorphic non-returning transition
```

`Bit` is SEAM’s branch predicate type. Branch opcodes consume `Bit` (`0` branches, `1` falls through). Integers are never reinterpreted as predicates.

## Method signatures

A SEAM function has:

```text
parameter/local descriptors
result stack type list
block table
bytecode body
root map
```

Method results are stack lists, not a single return type.

Example:

```text
.method pair(x:Int, y:Bit) -> [Int, Bit]
entry stack []:
  ld.loc 0
  ld.loc 1
  ret
```

No tuple allocation occurs for multi-result signatures. If a storable tuple/product is needed, lowering emits `new.obj`.

## Block signatures

Each basic block has an exact incoming stack signature.

Example:

```text
left stack []:
  ld.loc 0
  br join

right stack []:
  ld.loc 1
  br join

join stack [Int]:
  ret
```

A branch transfers the whole current stack after popping its branch operand (`Bit` for `br.z`, index for `br.tbl`). There is no partial branch payload convention.

## Branch stack rule

For unconditional branch:

```text
br target
```

Verifier rule:

```text
current stack must exactly equal target incoming stack
```

For `br.z` branch:

```text
br.z target
```

Verifier rule:

```text
current stack must be target incoming stack + Bit on top
top `Bit` is popped
remaining stack must exactly match target incoming stack
```

For table branch:

```text
br.tbl table
```

Verifier rule:

```text
current stack must be common target incoming stack + Int index on top
index is popped
all possible targets must have exactly the same incoming stack
```

If different branches need different payloads, the frontend must materialize them in locals/objects before branching.

## Verifier algorithm sketch

For each function:

```text
1. Read parameter/local descriptors and result stack list.
2. Initialize entry block with declared incoming stack.
3. For each block, run instructions linearly.
4. At each instruction, check current stack suffix against required inputs.
5. Pop required inputs, push outputs.
6. At branch, check target incoming stack exactly.
7. At ret, check current stack exactly equals function result stack.
8. Emit verified root-map points from typed locals/stacks at safe points.
9. Reject unknown, ambiguous, or guessed stack transitions.
```

The verifier must not infer hidden conversions or truthiness.

## Numeric operations

Public arithmetic opcodes should be generic enough to avoid a Java-like public opcode explosion, but constrained enough for the verifier.

Core public operations:

```text
add
sub
mul
div.s
rem.s
and
or
xor
not
cmp.eq
cmp.ne
cmp.lt
cmp.le
cmp.gt
cmp.ge
```

Shift/rotate source syntax should be named functions via UFCS. SEAM may still have compact VM mnemonics if they are primitive transitions:

```text
shl
shr
sar
rol
ror
```

These are SEAM opcodes if and only if the VM treats them as primitive value transitions. They are not Musi source keywords.

## Constants and literals

Constants should load through generic constant-table instructions and a small set of compact immediates.

Examples:

```text
ld.c c
ld.c.i4 small
ld.str s
ld.zero type
ld.one type
```

Whether `ld.zero` / `ld.one` exist is an encoding choice. They denote `0`/`1` bit-or-word constants, not source keyword spelling.

## Error and trap model at bytecode level

SEAM should have a low-level trap/non-return transition for verified fatal paths and runtime failures.

```text
trap reason
```

Possible stack effect:

```text
[; Never]
```

or verifier rule:

```text
terminates current block with no successor
```

Musi source failures use `Expect[T,E]`. `trap` is not source failure handling. It is VM/runtime failure.

## No exceptions in core SEAM

SEAM should not add exception tables as default control. Source `Expect` and `Maybe` lower to data. Cleanup lowers through explicit exit paths or runtime frame cleanup records. If a host ABI can throw, `@foreign`/external descriptors must specify the boundary behavior; source does not gain `throw`/`catch`.

## Diagnostics

Verifier diagnostics should report stack effects directly:

```text
error: stack mismatch at branch target `join`
current after pop: [Word, Bit]
target expects:    [Word]
```

```text
error: `br.z` requires Bit on top of stack
found: Int
```

```text
error: `ret` stack does not match function result
found:    [Word]
expected: [Expect[Word, IOError]]
```

## Freeze checklist

Freeze these before opcode numbers:

```text
[x] rightmost-is-top convention
[x] block signature syntax
[x] method result stack syntax
[x] branch stack rule
[x] table-branch common-target-stack rule
[x] display segment naming convention
[x] public/internal opcode split
[x] no `.seamil` peer artifact rule
```
