# SEAM Runtime, GC, Root Maps, `defer`, `pin`, `yield`, and Coroutines

## Set-in-Stone Header

- Set-in-stone track: `docs/__smallcore__/PLAN.md`
- Set-in-stone status: frozen 0.1.0 baseline active as of `2026-05-14`.
- Reconciliation source: `docs/__smallcore__/reconciliation.md`

Status: normative freeze document (0.1.0 baseline).

Covers:

```text
5. GC root maps and Immix/mark-region runtime
6. defer/pin/yield lowering
7. coroutine frame representation
```

The runtime should be small enough to embed, but not naive. Musi’s design exposes `pin`, `defer`, and `yield` because those operations affect GC, frames, roots, cleanup, suspension, and host boundaries in ways ordinary library calls cannot express.

## Runtime identity

SEAM runtime state includes:

```text
loaded module/package images
function/layout/type/external descriptors
constant pool values
operand stack / frame stack
locals and frame metadata
managed heap
GC metadata
root maps
pin leases
defer cleanup records or lowered cleanup state
coroutine states
scheduler/driver integration points
external/native boundary handlers
```

The runtime should support modes:

```text
debug interpreter
normal interpreter
quickened interpreter
fused-dispatch/kernel path
hot/embedder path
```

Mode changes must not change public `.seam` semantics.

## GC direction

Use a generational mark-region / Immix-family collector.

Recommended shape:

```text
young generation
  bump allocation
  frequent minor collection
  remembered-set/card-table support

mature generation
  Immix-like blocks and lines
  mark-region tracing
  opportunistic evacuation/defragmentation if implemented

large object space
  separate large allocation handling
  conservative movement policy for very large objects

pinned object handling
  pin counters/leases/flags
  pinned blocks handled carefully to avoid compaction hazards
```

The current repo already tracks generational Immix/mark-region direction and VM modes. This document turns that into runtime contract language.

## Managed object header

Suggested object header fields:

```text
layout id / type id
mark bits
generation bits
flags: pinned, remembered, large, weak-capable, forwarding, etc.
size or payload length when needed
```

Object body:

```text
fields or array payload
```

Layout descriptor supplies:

```text
field count and types
pointer bitmap
mutability flags
alignment
variant tag information
finalization/cleanup flags if any
```

Do not use hidden destructor/finalizer semantics for ordinary Musi cleanup. `defer` is explicit.

## Allocation

Allocation paths:

```text
new.obj
new.arr
new.fn / closure env
runtime helper allocation
```

The allocator should fast-path small fixed-layout objects and arrays. Runtime modes may route through typed kernels or fused allocation paths.

Allocation failure policy must be explicit. It should not silently become source exceptions. Source-level allocation helpers should return `Expect` where failure is user-visible, or trap only when the API states it.

## GC barriers

The runtime needs write barriers for old-to-young references.

Barrier triggers:

```text
st.fld when writing managed reference into managed object
st.elem when writing managed reference into array
closure env writes if mutable env cells exist
module/global writes when managed references cross generation
```

The verifier and layout descriptors should know which writes can contain roots.

## Root maps

Root maps are required at safe points:

```text
call
call.ind
call.ffi
allocation
possible collection point
pin helper entry/exit
yield/suspension
trap/error handling path if stack may be inspected
```

Root-map entry:

```text
function id
pc/block offset
local root bitmap / typed root list
operand stack root bitmap / typed root list
capture/env root map if applicable
active defer record root descriptors
active pin lease descriptors
```

The verifier already has typed locals and stack types; use that to generate precise root maps.

## Stack and root invariant

At every safe point:

```text
all managed references in locals are known
all managed references on operand stack are known
all captured references in closures/coroutines are known
all defer-captured values are known
all pin leases are known
```

No unknown raw pointer can be treated as a managed root. Raw `Ptr[T]` is unsafe/native-domain data, not a GC reference.

## Pinning

Source:

```musi
pin value as name in expr
```

Meaning:

```text
create a scoped stable-address lease for a managed value or buffer
bind a raw/native-facing address or pinned view for the body
release the lease on all body exits
```

`pin` is a source keyword because a library call cannot safely create scoped address stability with verifier/GC knowledge.

## Pin lowering

Lowering strategy:

```text
1. evaluate subject
2. call runtime/native pin helper or emit pin lease transition
3. bind pinned view/raw address in a local
4. evaluate body
5. release lease on every exit path
```

Possible lowered shape:

```text
let __pin0 := Runtime_pin(buffer);
match __pin0 (...)
...
Runtime_unpin(__pin0);
```

or direct SEAM transitions if pin/unpin are public bytecode ops:

```text
pin leaseDescriptor
...
unpin leaseDescriptor
```

The public decision can be runtime-helper lowering rather than opcodes. The important invariant is verifier/GC visibility.

## Pin rules

```text
pin is lexical
pinned address cannot escape the pin body
no yield across active pin lease
no closure capture of pinned raw address
no storing pinned raw address in managed heap
foreign calls may receive pinned addresses only while lease is active
```

Default rule:

```text
yield is forbidden inside an active pin region
```

If a future non-suspending yield category exists, it must be proved not to suspend. The frozen small-core rule rejects yield inside pin.

## Raw pointer boundary

Raw pointers:

```text
Ptr[T]
```

are unsafe, non-null, typed raw pointers. Nullable raw pointers use:

```text
Maybe[Ptr[T]]
```

Raw pointer operations remain method-shaped or runtime-helper-shaped:

```musi
ptr.load()
ptr.store(value)
ptr.cast[U]()
ptr.addr()
```

No C-style pointer arithmetic opcodes are required in public SEAM.

## `defer`

Source:

```musi
defer cleanup;
defer cleanup where guard;
```

Meaning:

```text
register cleanup now
run cleanup on scope exit
run in reverse registration order
guard is evaluated at cleanup time
cleanup result must be () or be explicitly ignored/handled
```

`defer` is the source word. `unwind` is a VM/spec/runtime term, not source syntax.

## Defer lowering

There are two valid implementation strategies.

### Strategy A: static explicit cleanup insertion

Frontend inserts cleanup calls at every exit path.

Source:

```musi
let file := open(path);
defer close(file);
body
```

Lowering sketch:

```musi
let file := open(path);
let __result := body;
close(file);
__result
```

For multiple exits, each exit receives the cleanup sequence.

Pros:

```text
no runtime cleanup stack in ordinary cases
bytecode shows explicit calls/branches
easy decompilation into raw lowered Musi
```

Cons:

```text
code duplication
harder with many exits/suspension/cancel unless normalized carefully
```

### Strategy B: runtime cleanup records

Runtime/frame records store deferred cleanup actions.

Pros:

```text
centralized cancellation/drop behavior
natural with coroutine frames
less code duplication
```

Cons:

```text
more runtime machinery
cleanup closure/capture representation must be explicit
```

Recommended policy:

```text
ordinary non-suspending blocks: prefer static explicit cleanup insertion
coroutine/yield-capable frames: allow runtime cleanup records if needed
```

Public semantics are the same.

## Defer and `let ... else`

`let pattern := expr else fallback;` exits the surrounding block on failure. Deferred cleanups registered before the let-else must run before fallback result leaves the block.

Example:

```musi
let file := open(path);
defer close(file);
let .Success(bytes) := read(file) else .Failure(.ReadFailed);
parse(bytes)
```

Failure path runs `close(file)`.

## Defer and tail calls

A tail call cannot skip pending defers.

Either:

```text
cleanup has already been emitted before tail call
```

or:

```text
tail call is rejected while cleanup records are pending
```

Default: reject or de-tailcall when pending defers exist.

## Defer and cleanup failure

Cleanup should not implicitly replace the surrounding result. If cleanup may fail, the cleanup expression must handle it explicitly.

Example:

```musi
defer match close(file) (
| .Success(_) => ()
| .Failure(e) => logCloseFailure(e)
);
```

No hidden exception/finally semantics.

## `yield`

Source:

```musi
let reply := yield request;
```

Meaning:

```text
suspend current coroutine/frame
emit request to driver
later resume with reply value
```

`yield` is a keyword because a library function cannot suspend the current SEAM frame while preserving root maps, locals, stack shape, defers, and pin rules.

## `yield` type model

A coroutine has three type parameters conceptually:

```text
Coroutine[Yield, Resume, Return]
```

Inside such a coroutine:

```text
yield : Yield -> Resume
```

Aliases may exist in std/runtime:

```text
Generator[Y, R] = Coroutine[Y, (), R]
Task[T] = Coroutine[TaskRequest, TaskReply, T]
```

But `Task`, `Generator`, `Scheduler`, and `spawn` are library/runtime protocols, not source keywords.

## Yield lowering

Two valid lowering styles:

### Style A: explicit state machine

Frontend rewrites coroutine into state object plus resume function.

Frame object fields:

```text
state tag
locals
saved stack values if needed
pending defer records
result/reply slot
driver/protocol id
```

`yield` becomes:

```text
store current state
return request to driver
resume enters block by state tag
```

### Style B: runtime frame suspension

VM captures a SEAM frame segment.

Frame snapshot:

```text
function id
pc/block id
locals
operand stack slice
root-map id
defer records
runtime state
```

This is simpler at source lowering but heavier in runtime.

Recommendation:

```text
public semantics allow either
release runtime may use frame suspension or lowered state machines
SEAM verifier must describe roots and stack shape either way
```

## Coroutine frame representation

Coroutine frame/state object must include:

```text
function id or resume method id
resume state/block id
local slot vector or state fields
saved operand stack values, if any
active defer records
active driver/protocol descriptor
reply slot type
return/result slot type
root-map descriptor
state: ready | suspended | done | cancelled
```

Cancellation/drop rules:

```text
running coroutine completes normally -> defers run exactly once
cancelled suspended coroutine -> defers run exactly once
dropped completed coroutine -> no cleanup rerun
panic/trap recoverable path -> defers run if runtime supports recovery through frame cleanup
```

## Yield verifier rules

At every yield point:

```text
current stack shape is known
request expression type matches coroutine Yield type
resume result type matches coroutine Resume type
no active pin lease exists
no borrow-only safe view survives suspension
root map for locals/stack/defer captures exists
```

Types that must not survive yield:

```text
Ref[T]
MutRef[T]
Slice[T]
raw pinned address
borrow-only view
```

If the source wants to keep data across yield, it must own/copy it into a managed value.

## `spawn` and `await`

These are not source keywords in the frozen small-core SEAM design.

Reason:

```text
yield is the primitive suspension contract.
spawn is scheduler authority and policy, so it belongs to a Scheduler capability object.
await is a task protocol over yield, so it belongs to library/runtime helpers.
```

Example:

```musi
let handle := scheduler.spawn(work);
let result := wait(handle);
```

`wait` may internally yield a scheduler request.

## GC interaction with coroutine frames

Coroutine frames are managed objects or managed runtime records. They must be root-map-describable.

If a coroutine is suspended, its frame is a root when reachable from scheduler/handle. If it is unreachable, GC may collect it after running required cleanup or by making cleanup part of final reachable cancellation protocol.

Recommended rule:

```text
Dropping/cancelling coroutine handles must have explicit runtime semantics.
GC finalization should not be the primary cleanup path.
```

`defer` is deterministic. Do not rely on finalizers.

## Root map for coroutine state

For lowered state-machine style, root maps attach to state object layout.

For suspended-frame style, root maps attach to:

```text
function id + resume pc + stack/local descriptors
```

Both must enumerate:

```text
managed references in locals
managed references in saved stack
managed references in defer captures
managed references in driver/request/reply slots
```

## Runtime modes and quickening

Runtime may quicken public instructions after verification.

Examples:

```text
ld.fld -> ld.fld.ref.off8
call -> call.direct.inlineCache
new.obj -> new.obj.fast.layoutK
```

Quickened/internal forms are not serialized as public `.seam` and do not change decompilation semantics.

## Step/stack/heap limits

Runtime should support embedders:

```text
heap limit
stack/frame limit
instruction/step budget
GC stress mode
runtime tier/mode flags
```

Budget failures should produce runtime traps or `Expect`-returning host API failures depending on API. Source-level exceptions are not introduced.

## Freeze checklist

```text
[x] object header fields
[x] root-map safe-point requirements
[x] write barrier triggers
[x] pin lexical rules
[x] yield-forbidden-inside-pin rule
[x] defer LIFO and guard-at-exit rules
[x] cleanup failure policy
[x] coroutine frame fields
[x] cancellation/drop cleanup rules
[x] spawn/await not-keyword rule
```
