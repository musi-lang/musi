# Musi Runtime Interop Model

Status: proposed

This spec defines runtime address and unsafe boundary rules that support managed views, raw pointers, and pin regions.

## Safe Address Types

`Ref[T]` is shared, non-null, borrow-only safe reference.

Properties:

- read access only
- any number of shared borrows may coexist
- may not be stored, returned, or closure-captured
- may not survive handler suspension or resume

`MutRef[T]` is exclusive, non-null, borrow-only writable safe reference.

Properties:

- writable access to referent
- exclusive for lifetime
- may not be stored, returned, or closure-captured
- may not survive handler suspension or resume

`Slice[T]` is safe, non-null, borrow-only contiguous view.

Properties:

- no ownership
- carries contiguous traversal semantics
- follows borrow lifetime rules
- replaces pointer arithmetic for ordinary traversal

`Array[T]` remains owner. `Slice[T]` remains view.

## Rust Comparison

Rust 2024 references, raw pointers, `unsafe`, and pinning are implementation reference points for this runtime layer.

Musi does not adopt Rust surface syntax:

- no `&T` / `&mut T` source notation
- no `*const T` / `*mut T` source notation
- no Rust lifetime parameters as user-facing borrow syntax
- no Rust `Pin<T>` spelling as Musi source syntax

Musi exposes semantic consequences as `Ref[T]`, `MutRef[T]`, `Slice[T]`, `Ptr[T]`, explicit unsafe capability, and runtime/native pin regions.

## Raw Address Type

`Ptr[T]` is unsafe, non-null raw typed pointer.

Properties:

- usable only inside `unsafe (...)`
- no ambient null
- nullable raw pointers use `Option[Ptr[T]]`
- no infix pointer arithmetic
- no raw pointer indexing syntax

Raw pointer movement through memory does not use C-style arithmetic. Ordinary traversal uses `Slice[T]`.

## Unsafe Form

Unsafe code is an expression boundary, not a new ownership system.

## Raw Pointer Operations

Raw pointer operations use methods, not sigils.

Reserved operation family:

- `ptr.load()`
- `ptr.store(value)`
- `ptr.cast[U]()`
- `ptr.addr()` only where integer address exposure is explicitly allowed

This spec does not add `ptr.add`, `ptr.offset`, or C-style `*ptr` / `ptr[index]`.

## Pinning

Stable-address scope is lexical and explicit. Address stability ends with scope.

Managed objects may move, so raw-address interop with managed storage needs runtime/native pin support or copied buffers.

## Slice Construction

`Slice[T]` construction should reuse indexing and slicing syntax, not introduce separate keyword-only container creation.

Exact slice grammar remains defined by the canonical grammar files:

- `grammar/Musi.abnf`
- `grammar/MusiParser.g4`

## Explicit Non-Goals

This spec rejects:

- array-to-pointer decay
- implicit borrow creation everywhere
- sigil-based borrow syntax such as `&x`
- full C-style pointer arithmetic
- nullable-by-default pointers
- storable safe borrows

## References

- C# `unsafe` keyword: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/unsafe
- C# `fixed` statement: https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/fixed
- Zig language documentation index: https://ziglang.org/documentation/master/
- Ada and SPARK reference material index: https://docs.adacore.com/
