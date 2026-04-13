# SEAM Domains

Status: proposed

This spec defines the final public SEAM domain contract.

Domain names are fixed:

- `managed`
- `resumable`
- `native`
- `link`
- `introspect`

These are VM and module-contract concepts. They are not source-language feature names.

## `managed`

`managed` defines movable managed heap semantics and verifier-tracked non-owning views.

Public features:

- `managed.core`
- `managed.layout`
- `managed.views`
- `managed.weak`
- `managed.rtti`

Standardized heap and value kinds:

- `string`
- `bytes`
- `array`
- `object`
- `closure`
- `continuation`
- `weakref`
- ephemeral `ref`
- ephemeral `mutref`
- ephemeral `slice`

Rules:

- heap objects may move
- stable managed addresses do not exist outside native pin regions
- `ref`, `mutref`, and `slice` are non-null and non-owning
- `mutref` is exclusive
- `ref`, `mutref`, and `slice` may not be stored in heap, globals, exports, closures, or returns
- maps, sets, tagged sums, tuples, ropes, and similar structures lower to these primitives instead of becoming VM-native heap kinds
- finalizers and destructor semantics are not part of `managed`

Relation to Musi specs:

- `Array[T]`, `Ref[T]`, `MutRef[T]`, and `Slice[T]` lower against this domain
- movable managed heap must remain consistent with [runtime-memory-model.md](/Users/krystian/CodeProjects/musi-next/specs/runtime-memory-model.md)

## `resumable`

`resumable` defines handler-driven resumable control.

Public features:

- `resumable.handlers`
- `resumable.cont.oneshot`
- `resumable.unwind`

Rules:

- handler frames are first-class runtime state
- continuation capture is one-shot
- resumed continuation may not resume again
- stack restoration and handler restoration are standardized runtime behavior
- multi-shot continuations are not part of this domain
- generators, coroutine schedulers, and async task systems are not part of this domain

This domain is named for runtime behavior, not source effect syntax.

## `native`

`native` defines host and ABI interop.

Public features:

- `native.abi.c`
- `native.abi.registry`
- `native.ptr`
- `native.pin`
- `native.cstr`
- `native.cptr`

Rules:

- `ptr<T>` is typed raw pointer capability
- `CString` and `CPtr` remain boundary mapping concepts
- ABI registry allows named ABIs beyond C
- nullability is explicit
- raw managed address extraction requires active pin scope
- C-style pointer arithmetic is not part of this domain
- ambient host powers are not part of this domain

Relation to Musi specs:

- `Ptr[T]`, `CString`, `CPtr`, `pin`, and `unsafe` lower against this domain
- this domain must remain consistent with [unsafe-and-addresses.md](/Users/krystian/CodeProjects/musi-next/specs/unsafe-and-addresses.md) and [c-interop-memory.md](/Users/krystian/CodeProjects/musi-next/specs/c-interop-memory.md)

## `link`

`link` defines import, export, capability, and module-loading contracts.

Public features:

- `link.imports`
- `link.exports`
- `link.capabilities`
- `link.dynamic`

Rules:

- static imports and exports are standardized
- capability imports are explicit and verified
- dynamic loading and lookup are domain features, not ambient VM behavior
- source-language module semantics are not part of this domain

This domain exists so runtime linking is a declared contract instead of a side effect of the host.

## `introspect`

`introspect` defines public metadata and public reflection.

Public features:

- `introspect.types`
- `introspect.members`
- `introspect.attrs`
- `introspect.layout`
- `introspect.invoke`
- `introspect.access`
- `introspect.construct`

Rules:

- reflection is full for public and exported contract only
- reflection may inspect metadata, invoke public members, access public members, and construct public values
- private or internal details are outside portable SEAM reflection contract
- source maps, debug names, and tool metadata live in this domain

This domain is wider than `debug`. It covers runtime-introspectable public structure as well as tooling payloads.

## Common Verification Rules

The SEAM verifier enforces:

- required domain presence
- required feature presence
- stack and local discipline
- non-escaping view restrictions for `managed.views`
- one-shot continuation discipline for `resumable.cont.oneshot`
- pin requirements for `native.pin`
- declared capability and import correctness for `link`
- public/export-only visibility boundaries for `introspect`

## Feature Naming

Feature ids use `domain.feature` naming.

Rules:

- domain segment is reserved
- feature segment is stable and additive where possible
- modules declare required features explicitly

## See Also

- [seam-format.md](/Users/krystian/CodeProjects/musi-next/specs/seam-format.md)
- [runtime-memory-model.md](/Users/krystian/CodeProjects/musi-next/specs/runtime-memory-model.md)
- [unsafe-and-addresses.md](/Users/krystian/CodeProjects/musi-next/specs/unsafe-and-addresses.md)
