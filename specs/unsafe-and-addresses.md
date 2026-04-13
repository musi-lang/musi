# Musi Unsafe And Address Model

Status: proposed

This spec defines proposed source-language address types and unsafe forms.

## Safe Address Types

### `Ref[T]`

`Ref[T]` is shared, non-null, borrow-only safe reference.

Properties:

- read access only
- any number of shared borrows may coexist
- may not be stored, returned, or closure-captured
- may not survive handler suspension or resume

### `MutRef[T]`

`MutRef[T]` is exclusive, non-null, borrow-only writable safe reference.

Properties:

- writable access to referent
- exclusive for lifetime
- may not be stored, returned, or closure-captured
- may not survive handler suspension or resume

`MutRef[T]` exists as separate type name because `mut T` alone does not communicate exclusivity.

### `Slice[T]`

`Slice[T]` is safe, non-null, borrow-only contiguous view.

Properties:

- no ownership
- carries contiguous traversal semantics
- follows borrow lifetime rules
- replaces pointer arithmetic for ordinary traversal

`Array[T]` remains owner. `Slice[T]` remains view.

## Raw Address Type

### `Ptr[T]`

`Ptr[T]` is unsafe, non-null raw typed pointer.

Properties:

- usable only inside `unsafe (...)`
- no ambient null
- nullable raw pointers use `Option[Ptr[T]]`
- no infix pointer arithmetic
- no raw pointer indexing syntax

Raw pointer movement through memory does not use C-style arithmetic. Ordinary traversal uses `Slice[T]`.

## Construction Forms

### Borrow Creation

Proposed borrow forms:

```musi
let r := borrow value;
let w := borrow mut value;
```

These are keyword forms, not function-call syntax and not sigil syntax.

### Unsafe Form

Proposed unsafe expression form:

```musi
let x := unsafe (ptr.load());
```

### Pinning

Proposed pin form:

```musi
pin bytes as pinned => unsafe (
  nativeWrite(pinned.ptr());
);
```

Pinned scope is lexical and explicit. Address stability ends with scope.

## Raw Pointer Operations

Raw pointer operations use methods, not sigils.

Reserved operation family:

- `ptr.load()`
- `ptr.store(value)`
- `ptr.cast[U]()`
- `ptr.addr()` only where integer address exposure is explicitly allowed

This spec does not add `ptr.add`, `ptr.offset`, or C-style `*ptr` / `ptr[index]`.

## Slice Construction

`Slice[T]` construction should reuse indexing and slicing syntax, not introduce separate keyword-only container creation.

That means Musi should extend existing index/slice grammar rather than teach a second contiguous-view creation language.

Example direction:

```musi
let middle := values.[2 ..< 6];
```

Exact slice grammar remains proposed and must align with canonical grammar files:

- [grammar/Musi.abnf](/Users/krystian/CodeProjects/musi-next/grammar/Musi.abnf)
- [grammar/MusiParser.g4](/Users/krystian/CodeProjects/musi-next/grammar/MusiParser.g4)

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
