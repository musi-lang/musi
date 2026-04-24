let core := import "musi:core";

--- Null-terminated C string ABI value.
export let CString := core.CString;

--- Raw C pointer ABI value.
export let CPtr := core.CPtr;

--- Scoped pinned value handle created by `pin` inside `unsafe`.
export let Pin := core.Pin;

--- C char ABI type.
export let CChar := core.Int8;
--- C signed char ABI type.
export let CSChar := core.Int8;
--- C unsigned char ABI type.
export let CUChar := core.Nat8;
--- C signed short ABI type.
export let CShort := core.Int16;
--- C unsigned short ABI type.
export let CUShort := core.Nat16;
--- C signed int ABI type.
export let CInt := core.Int32;
--- C unsigned int ABI type.
export let CUInt := core.Nat32;
--- C signed long ABI type.
export let CLong := core.Int;
--- C unsigned long ABI type.
export let CULong := core.Nat;
--- C signed long long ABI type.
export let CLongLong := core.Int64;
--- C unsigned long long ABI type.
export let CULongLong := core.Nat64;
--- C size_t ABI type.
export let CSize := core.Nat;
--- C ptrdiff_t ABI type.
export let CSizeDiff := core.Int;
--- C float ABI type.
export let CFloat := core.Float32;
--- C double ABI type.
export let CDouble := core.Float64;

--- Typed view over a raw C pointer.
export opaque let Ptr [T] := data {
  | RawPtr(raw : CPtr)
};

native "musi" let ptrNullIntrinsic () : CPtr;
native "musi" let ptrIsNullIntrinsic (raw : CPtr) : Bool;

--- Cast raw pointer into a typed pointer view.
export let cast [T] (raw : CPtr) : Ptr[T] := .RawPtr(raw := raw);

--- Null pointer.
export let null [T] () : Ptr[T] := .RawPtr(raw := unsafe {
    ptrNullIntrinsic();
  });

--- Return raw pointer value.
export let raw [T] (pointer : Ptr[T]) : CPtr :=
  match pointer (
  | .RawPtr(raw := raw) => raw
  );

--- Returns true when pointer is null.
export let isNull [T] (pointer : Ptr[T]) : Bool := unsafe {
  ptrIsNullIntrinsic(raw[T](pointer));
};

--- Foreign pointer operations.
export native "musi" (
  let offset [T] (pointer : Ptr[T], count : Int) : Ptr[T];
  let read [T] (pointer : Ptr[T]) : T;
  let write [T] (pointer : Ptr[mut T], value : T) : Unit;
);

--- Pointer helper namespace.
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
