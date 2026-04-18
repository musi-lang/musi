@known(name := "Type")
export let Type := Type;

@known(name := "Array")
export let Array := Array;

@known(name := "Any")
export let Any := Any;

@known(name := "Unknown")
export let Unknown := Unknown;

@known(name := "Syntax")
export let Syntax := Syntax;

@known(name := "Empty")
export let Empty := Empty;

@known(name := "Unit")
export let Unit := Unit;

@known(name := "Bool")
export let Bool := Bool;

@known(name := "Nat")
export let Nat := Nat;

@known(name := "Int")
export let Int := Int;

@known(name := "Int8")
export let Int8 := Int8;

@known(name := "Int16")
export let Int16 := Int16;

@known(name := "Int32")
export let Int32 := Int32;

@known(name := "Int64")
export let Int64 := Int64;

@known(name := "Nat8")
export let Nat8 := Nat8;

@known(name := "Nat16")
export let Nat16 := Nat16;

@known(name := "Nat32")
export let Nat32 := Nat32;

@known(name := "Nat64")
export let Nat64 := Nat64;

@known(name := "Float")
export let Float := Float;

@known(name := "Float32")
export let Float32 := Float32;

@known(name := "Float64")
export let Float64 := Float64;

@known(name := "String")
export let String := String;

@known(name := "Rune")
export let Rune := Rune;

@known(name := "Range")
export let Range := Range;

@known(name := "ClosedRange")
export let ClosedRange := ClosedRange;

@known(name := "PartialRangeFrom")
export let PartialRangeFrom := PartialRangeFrom;

@known(name := "PartialRangeUpTo")
export let PartialRangeUpTo := PartialRangeUpTo;

@known(name := "PartialRangeThru")
export let PartialRangeThru := PartialRangeThru;

@known(name := "CString")
export let CString := CString;

@known(name := "CPtr")
export let CPtr := CPtr;

export opaque let Option [T] := data {
  | Some(T)
  | None
};

export let some [T] (value : T) : Option[T] := .Some(value);

export let none [T] () : Option[T] := .None;

export let isSome [T] (value : Option[T]) : Bool := match value (
  | .Some(item) => 0 = 0
  | .None => 0 = 1
);

export let isNone [T] (value : Option[T]) : Bool := match value (
  | .Some(item) => 0 = 1
  | .None => 0 = 0
);

export let unwrapOr [T] (value : Option[T],
  fallback : T
) : T := match value (
  | .Some(item) => item
  | .None => fallback
);

export opaque let Abort := effect {
  let abort () : Empty;
};

export let abort () : Empty := request Abort.abort();

export opaque let Rangeable[T] := class {
  let compare (left : T, right : T) : Int;
  let next (value : T) : Option[T];
  let prev (value : T) : Option[T];
};

export opaque let RangeBounds[T] := class {
  let defaultLowerBound () : T;
  let defaultUpperBound () : T;
};

export instance Rangeable[Int]{
  let compare (left : Int, right : Int) : Int := left - right;
  let next (value : Int) : Option[Int] := some[Int] (value + 1);
  let prev (value : Int) : Option[Int] := some[Int](value - 1);
};

export instance RangeBounds[Int]{
  let defaultLowerBound () : Int := - 1000000;
  let defaultUpperBound () : Int := 1000000;
};
