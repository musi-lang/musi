--! Foreign function interface pointer and ABI helpers.

let ffiHost := import "musi:ffi";

--- Null-terminated C string ABI value.
export let CString := ffiHost.CString;

--- Raw C pointer ABI value.
export let CPtr := ffiHost.CPtr;

--- Scoped pinned value handle created by `pin` inside `unsafe`.
export let Pin := ffiHost.Pin;

--- C char ABI type.
export let CChar := Int8;
--- C signed char ABI type.
export let CSChar := Int8;
--- C unsigned char ABI type.
export let CUChar := Nat8;
--- C signed short ABI type.
export let CShort := Int16;
--- C unsigned short ABI type.
export let CUShort := Nat16;
--- C signed int ABI type.
export let CInt := Int32;
--- C unsigned int ABI type.
export let CUInt := Nat32;
--- C signed long ABI type.
export let CLong := Int;
--- C unsigned long ABI type.
export let CULong := Nat;
--- C signed long long ABI type.
export let CLongLong := Int64;
--- C unsigned long long ABI type.
export let CULongLong := Nat64;
--- C size_t ABI type.
export let CSize := Nat;
--- C ptrdiff_t ABI type.
export let CSizeDiff := Int;
--- C float ABI type.
export let CFloat := Float32;
--- C double ABI type.
export let CDouble := Float64;
--- Typed view over a raw C pointer.
export hidden let Ptr [T] := data {
  | RawPtr(raw : CPtr)
};

--- Cast raw pointer into a typed pointer view.
export let cast [T] (raw : CPtr) := .RawPtr(raw := raw);

--- Null pointer.
export let null [T] () := .RawPtr(raw := ffiHost.ptrNull());

--- Return raw pointer value.
export let raw [T] (pointer : Ptr[T]) :=
  match pointer (
  | .RawPtr(raw := raw) => raw
  );

--- Returns true when pointer is null.
export let isNull [T] (pointer : Ptr[T]) := ffiHost.ptrIsNull(raw[T](pointer));

--- Foreign pointer operations.
@foreign(abi := .musi)
--- Offset pointer by element count.
export let offset [T] (pointer : Ptr[T], count : Int) : Ptr[T];
@foreign(abi := .musi)
--- Read value from pointer target.
export let read [T] (pointer : Ptr[T]) : T;
@foreign(abi := .musi)
--- Write value into mutable pointer target.
export let write [T] (pointer : Ptr[mut T], value : T) : Unit;

--- Pointer tools.
export let ptr :=
  {
    cast := cast,
    isNull := isNull,
    null := null,
    offset := offset,
    raw := raw,
    read := read,
    write := write,
  };
