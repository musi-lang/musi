# Musi Runtime Memory Model

Status: proposed

This spec defines Musi's runtime memory law.

It does not claim current implementation completeness. Current repo facts that inform this spec include:

- `Array`, `CString`, and `CPtr` already exist in `crates/musi_foundation/modules/core.ms`
- `CString` and `CPtr` are re-exported by `packages/std/prelude/index.ms`
- current VM values include `CPtr` and heap-backed data in `crates/musi_vm/src/value.rs`

## Core Law

Musi uses one managed-core memory model.

Safe Musi code lives in managed storage. Managed objects may move. Source language semantics must not depend on stable managed addresses unless code enters an explicit runtime/native pin region.

Musi separates four categories:

- owning managed values such as `Array[T]`
- safe borrows such as `Ref[T]`, `MutRef[T]`, and `Slice[T]`
- unsafe raw addresses such as `Ptr[T]`
- C interop mapping types such as `CString` and `CPtr`

Musi does not conflate arrays with pointers. Arrays do not decay into pointers. Slices do not decay into pointers. Raw pointers do not silently gain array semantics.

## Rust Comparison

Rust 2024 is the host implementation language, so Rust ownership and aliasing rules are the main implementation reference for safe runtime code.

Musi does not expose Rust lifetimes, borrow sigils, or `unsafe` block syntax as source authority. `Ref[T]`, `MutRef[T]`, and `Slice[T]` describe Musi consequences:

- shared read view
- exclusive writable view
- contiguous borrowed view
- no escaping borrowed views
- no stable managed address without runtime/native pin support

Rust references may implement or protect these runtime invariants, but Musi docs must state the Musi law rather than Rust syntax.

## Nullability

Null is never an ambient inhabitant of reference-like types.

Rules:

- `Ref[T]`, `MutRef[T]`, `Slice[T]`, and `Ptr[T]` are non-null
- nullable address results use `Option[...]`
- C-facing nulls are translated explicitly at the interop boundary

## Ownership And Aliasing

`Array[T]` is owning managed contiguous storage.

`Ref[T]` is shared borrow-only safe access.

`MutRef[T]` is exclusive borrow-only writable safe access.

`Slice[T]` is borrow-only safe contiguous view.

Borrow escape is illegal in safe code. Borrowed values may not be:

- stored in long-lived aggregates
- returned from functions
- captured by closures
- retained across handler suspension or resume boundaries

Alias law:

- any number of `Ref[T]` borrows may coexist
- `MutRef[T]` is exclusive for its lifetime
- `Slice[T]` follows same borrow law as other safe views
- mutation through alias needs exclusive writable borrow or explicit cell/handle types defined elsewhere

## Movement And Pinning

Managed objects may move during runtime operation.

Raw-address interop with managed objects therefore needs explicit runtime/native pinning or copied buffers.

Stable-address scope is a runtime contract, not a source syntax promise.

## Concurrency Law

Musi adopts DRF-SC.

Meaning:

- concurrent safe code is allowed
- data races are illegal
- data-race-free Musi programs behave as if operations were sequentially consistent

Unsynchronized shared mutable access must therefore be either:

- impossible in safe code
- mediated by explicit synchronization primitives
- or marked unsafe

This spec does not bless a weak memory model for ordinary safe code.

## Unsafe Boundary

Unsafe capabilities stay narrow and explicit.

Safe code must not require unsafe for ordinary high-performance container and range work.

## Proposed Public Types

This spec reserves these names for public language use:

- `Array[T]`
- `Ref[T]`
- `MutRef[T]`
- `Slice[T]`
- `Ptr[T]`
- `CString`
- `CPtr`

`CString` and `CPtr` are not general Musi pointer vocabulary. They belong to the C mapping layer.

## References

- Walter Bright, “The Biggest Mistake in C”: https://digitalmars.com/articles/C-biggest-mistake.html
- Go memory model: https://go.dev/ref/mem
- Nim memory management overview: https://nim-lang.org/1.6.20/mm.html
- D language specification index: https://dlang.org/spec/spec.html
