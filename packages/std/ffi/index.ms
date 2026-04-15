let Core := import "musi:core";

export let CString := Core.CString;
export let CPtr := Core.CPtr;
export let CChar := Core.Int;
export let CSChar := Core.Int;
export let CUChar := Core.Int;
export let CShort := Core.Int;
export let CUShort := Core.Int;
export let CInt := Core.Int;
export let CUInt := Core.Int;
export let CLong := Core.Int;
export let CULong := Core.Int;
export let CLongLong := Core.Int;
export let CULongLong := Core.Int;
export let CSize := Core.Int;
export let CSizeDiff := Core.Int;
export let CFloat := Core.Float;
export let CDouble := Core.Float;

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
