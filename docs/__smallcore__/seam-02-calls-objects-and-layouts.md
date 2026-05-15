# SEAM Calls, Frames, Objects, Variants, and Layouts

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: frozen 0.1.0 baseline active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Status: normative freeze document (0.1.0 baseline).

Covers:

```text
3. function/call/frame model
4. object/variant/layout model
```

SEAM should be expressive enough to represent Musi values, functions, closures, modules, variants, records, capability objects, and erased shapes without becoming an OO VM or source-syntax VM.

## Function model

A SEAM function is a bytecode body plus descriptors.

Function descriptor fields:

```text
function id
optional public/export name
private/generated name when needed
parameter type list
local type list
result stack type list
entry block id
block table id
bytecode body id
root-map table id
domain requirements
calling convention: managed | ffi-wrapper | runtime-helper | loader-generated
visibility: private | module-export | external-export
```

The function result is a **stack type list**, not necessarily one value.

Example:

```text
fn addPair : [Int, Int ; Int]
```

At source level this may correspond to:

```musi
let addPair(a : Int, b : Int) : Int := (
  a + b
);
```

At SEAM level it is a stack transition with locals and stack results.

## Call families

Public call families:

```text
call        direct managed function call
call.ind    indirect first-class function/closure call
call.ffi    external/ABI call
call.tail   direct tail call
call.ind.tail or tail.ind, if direct encoding is needed
```

Display spelling uses canonical dotted mnemonics:

```text
call.ffi 3
```

The display string is not identity.

### Direct call

```text
call methodId
```

Stack effect:

```text
[args... ; results...]
```

Verifier checks:

```text
arguments match method descriptor
result stack is appended exactly
callee domains are allowed by caller/module descriptor
```

### Indirect call

```text
call.ind
```

Stack effect:

```text
[Fn, args... ; results...]
```

The callable value supplies the method/layout/capture descriptor. Verifier rules depend on the callable type.

### Foreign call

```text
call.ffi foreignId
```

Stack effect:

```text
[args... ; results...]
```

The foreign descriptor owns ABI, pointer, pin, nullability, and domain requirements.

### Tail call

Tail call is a verifier-visible call mode. It must not bypass `defer` cleanup unless the lowering has already emitted cleanup or the tail call is inside a block whose cleanup state is known empty.

Tail-call verifier requirements:

```text
current function has no pending cleanup records, or cleanup has been emitted
current stack has exactly callee args
callee results match current function results
no active pin lease crosses tail boundary
```

Tail calls are allowed, but cleanup correctness wins over tail-call convenience.

## Frame model

A runtime call frame contains:

```text
function id
current pc / block id
locals array
operand stack segment
active defer records or lowered cleanup state
active pin leases
root-map id / current safe-point id
domain permission mask
runtime mode flags
suspension marker if frame can yield
```

The operand stack may be segmented by frame or represented as one VM stack with frame base pointers. The public semantics are frame-local.

Frame invariants:

```text
locals have verifier-known types
operand stack has verifier-known type list at every instruction point
root maps can enumerate all managed references in locals and stack
pin leases are lexical and bounded
pending defers run exactly once on frame/block abandonment
```

## Function values and closures

Musi functions/closures are runtime first-class. SEAM represents them as function values.

Closure value shape:

```text
method id
capture environment object or inline capture vector
call signature descriptor
possibly domain/effect/suspension marker
```

Public construction:

```text
new.fn methodId,captureCount
```

Stack effect:

```text
[capture0, ... captureN ; Fn]
```

Captured values must be representable and root-mapped. Safe views such as `Ref[T]`, `MutRef[T]`, and `Slice[T]` must not be captured if their lifetime rules reject storage/capture.

## Environment layout

Closure environments use ordinary layout descriptors.

```text
layout kind: closureEnv
field count
field types
GC bitmap
mutability flags if fields can be written
```

A captured `mut` place must lower to a representation that preserves mutability semantics. Possible lowered strategies:

```text
capture by cell object
capture by frame slot only if non-escaping
capture by runtime mutable-place descriptor
```

The verifier needs the final representation, not source capture syntax.

## Object model

SEAM object/value construction should be layout-driven.

Core opcodes:

```text
new.obj layoutId, fieldCount
ld.fld fieldId
st.fld fieldId
```

`new.obj` consumes field values in layout order and pushes the object/value.

Example:

```text
ld.loc ptr
ld.loc len
new.obj Buffer 2
```

Source:

```musi
let Buffer := data {
  let ptr : Ptr[mut Byte];
  let len : Nat;
};
```

SEAM does not need a `record` opcode. It needs `new.obj` and a layout descriptor.

## Data records

Record/product data layout descriptor:

```text
kind: record
field count
field names if public or map/debug preserved
field type ids
field mutability / storage qualifiers
field offset or logical index
alignment
GC bitmap
visibility flags
```

Private field names may be mangled or omitted in release artifacts. Public exported non-hidden field names must be preserved because they are API.

## Sum/variant data

Variant data lowers to objects with tag/payload semantics.

Source:

```musi
let Expect[T, E] := data {
| Success(value : T)
| Failure(error : E)
};
```

Canonical descriptor options:

### Unified layout

```text
layout Expect
field tag : small int / Bit / Byte / Word depending arity
field payload0 : union-ish storage descriptor
```

### Per-case layout

```text
layout Expect.Success
case group Expect
tag 0
field value : T

layout Expect.Failure
case group Expect
tag 1
field error : E
```

Per-case layout is often cleaner for verifier and GC bitmaps. Unified layout can be an optimization. Public decompilation should preserve canonical variant semantics.

## Variant construction

Source:

```musi
.Success(bytes)
```

Lowering:

```text
ld.loc bytes
new.obj Expect.Success 1
```

or if unified layout:

```text
ld.c tagSuccess
ld.loc bytes
new.obj Expect 2
```

SEAM does not have a `success` or `expect` opcode.

## Variant matching

Source:

```musi
match result (
| .Success(bytes) => parse(bytes)
| .Failure(error) => .Failure(error)
)
```

Lowering:

```text
read tag or type/case descriptor
compare or table-branch
field extract
branch to arm body
```

Possible text sketch:

```text
ld.loc result
ld.fld tag
br.tbl expectTable

L_success stack []:
  ld.loc result
  ld.fld 0
  call parse
  ret

L_failure stack []:
  ld.loc result
  ld.fld 0
  new.obj Expect.Failure 1
  ret
```

The verifier checks that all arm result stacks match.

## Maybe and Expect

`Maybe[T]`, `Expect[T,E]`, `?T`, `E!T`, and `??` are not SEAM opcode families.

They lower to data/layout operations:

```text
?T             -> Maybe[T]
E!T            -> Expect[T,E]
maybe ?? fb    -> tag test + branch + fallback expression
```

Canonical variants:

```text
Maybe.Some
Maybe.None
Expect.Success
Expect.Failure
```

Built-in/library names may be preserved in decompiled output because they are semantic anchors.

## Arrays, slices, and elements

SEAM should distinguish owned arrays from views/slices through type/layout descriptors.

Core operations:

```text
new.arr
ld.elem
st.elem
ld.len
```

Stack effects:

```text
new.arr T,n        [items... ; Array[T]]
ld.elem            [Array[T], Int ; T]
st.elem            [Array[mut T], Int, T ;]
ld.len             [Array[T] ; Nat]
```

`Slice[T]` is a runtime view type. Safe slicing lowers through helper/descriptors, not raw pointer arithmetic.

Raw `Ptr[T]` operations are unsafe and method-shaped at source. SEAM can lower them to runtime/native helper calls or domain opcodes only if the native domain descriptor allows it.

## Mutability in layouts

Source mutability is local and layered:

```musi
Ptr[T]
mut Ptr[T]
Ptr[mut T]
mut Ptr[mut T]
```

SEAM descriptors need enough information to verify writes:

```text
slot mutability
field mutability
pointee mutability
view mutability
array element mutability
```

Assignment/write operations require a mutable place at the appropriate layer.

Examples:

```text
st.loc requires local slot mutable
st.fld requires field/place mutable
st.elem requires mutable element access
Ptr.store requires Ptr[mut T] and unsafe/native permission
```

If source requires `let x : mut Int := mut 0;`, lowering should mark local slot `x` as mutable and initialize it. The `mut` value constructor does not need to survive as an opcode if local metadata carries the result.

## Modules as records

Source modules lower as record-shaped values plus import/export metadata.

Public module descriptor:

```text
module id
export table
import table
domain requirements
layout table refs
function table refs
```

Dynamic module loading, if needed, should use action-first display names:

```text
ld.mod.dyn
ld.exp.dyn
```

## Capability objects

Capability objects are ordinary values, usually records or erased shapes.

Source:

```musi
let Logger := shape {
  let write(level : LogLevel, text : String) : IOError!();
};
```

Runtime representation:

```text
concrete record/object with functions
or erased shape value with payload + witness/dispatch table
```

SEAM does not need an `ability`, `effect`, or `capability` opcode.

A call like:

```musi
logger.write(.Info, "starting")
```

may lower to:

```text
Logger_write(logger, .Info, "starting")
```

or dispatch through an erased witness:

```text
ld.loc logger
ld.fld witness
ld.fld writeSlot
call.ind
```

## Erased shapes

`erased Shape` exposes runtime erasure cost.

Representation:

```text
payload value
shape/witness descriptor
function dispatch table or dictionary
layout/type identity if needed
```

Verifier rules:

```text
erased value operations go through descriptor-approved dispatch
dynamic calls preserve declared stack effects
erased payload roots are visible to GC
dispatch tables are metadata/objects visible to loader/runtime
```

`erased` is not a SEAM opcode. It is a type/layout/descriptor fact.

## Hidden representation

`hidden` affects API visibility and decompilation/name retention. It does not require a special bytecode transition.

Rules:

```text
export hidden type name may be preserved
hidden fields/private constructors may be omitted or mangled without map
runtime layout still exists for execution and GC
```

## Layout optimization vs canonical semantics

The layout table may represent optimizations:

```text
niche/tag packing
inline small objects
unboxed scalar wrappers
specialized arrays
field reordering where permitted
```

But lowered Musi decompilation should project canonical semantics, not necessarily raw physical layout.

For example, a niche-optimized `Maybe[Ptr[T]]` may physically use null/non-null representation internally, but decompilation should still show:

```musi
match value (
| .Some(x) => ...
| .None => ...
)
```

unless the output is low-level SEAM disassembly.

## Field and variant names under decompilation

Without maps:

Preserve:

```text
built-in names
std/no-std anchors needed for linking
public exported non-hidden names
external ABI names
```

Mangle:

```text
private functions
private data names
private field names
private variant names
locals
temporaries
private module aliases
```

Compiler-generated names use `__` namespace. User identifiers beginning with `__` are forbidden.

## Frame-object interaction

Frames hold locals and stack values. Object values live in managed heap or inline/value storage depending on layout policy.

The runtime must ensure:

```text
all managed object references in locals/stack/captures are root-map-visible
field writes run GC barriers if they can create old-to-young edges
pinned object state is visible to GC
variant/object descriptors carry GC bitmaps
```

## Freeze checklist

```text
[x] function descriptor fields
[x] call family names and effects
[x] tail-call cleanup rule
[x] closure representation
[x] record layout descriptor
[x] variant/case layout descriptor
[x] erased-shape representation
[x] module action-first opcode naming
[x] hidden/decompilation name policy
```
