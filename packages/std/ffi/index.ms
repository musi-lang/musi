let core := import "musi:core";

export let CString := core.CString;
export let CPtr := core.CPtr;
export let CChar := core.Int8;
export let CSChar := core.Int8;
export let CUChar := core.Nat8;
export let CShort := core.Int16;
export let CUShort := core.Nat16;
export let CInt := core.Int32;
export let CUInt := core.Nat32;
export let CLong := core.Int;
export let CULong := core.Nat;
export let CLongLong := core.Int64;
export let CULongLong := core.Nat64;
export let CSize := core.Nat;
export let CSizeDiff := core.Int;
export let CFloat := core.Float32;
export let CDouble := core.Float64;

export opaque let Ptr[T] := data {
  | Ptr(raw : CPtr)
};

foreign "musi" let ptrNullIntrinsic () : CPtr;
foreign "musi" let ptrIsNullIntrinsic (raw : CPtr) : Bool;

export let cast [T] (raw : CPtr) : Ptr[T] := .Ptr(raw := raw);

export let null [T] () : Ptr[T] :=
  .Ptr(raw := unsafe { ptrNullIntrinsic(); });

export let raw [T] (pointer : Ptr[T]) : CPtr :=
  match pointer (
  | .Ptr(raw := raw) => raw
  );

export let isNull [T] (pointer : Ptr[T]) : Bool :=
  unsafe { ptrIsNullIntrinsic(raw[T](pointer)); };

export foreign "musi" (
  let offset[T] (pointer : Ptr[T], count : Int) : Ptr[T];
  let read[T] (pointer : Ptr[T]) : T;
  let write[T] (pointer : Ptr[mut T], value : T) : Unit;
);

export let ptr := {
  cast := cast,
  isNull := isNull,
  null := null,
  offset := offset,
  raw := raw,
  read := read,
  write := write
};
