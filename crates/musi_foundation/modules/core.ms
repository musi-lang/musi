@musi.lang(name := "Type")
export let Type := Type;

@musi.lang(name := "Array")
export let Array := Array;

@musi.lang(name := "Any")
export let Any := Any;

@musi.lang(name := "Unknown")
export let Unknown := Unknown;

@musi.lang(name := "Syntax")
export let Syntax := Syntax;

@musi.lang(name := "Empty")
export let Empty := Empty;

@musi.lang(name := "Unit")
export let Unit := Unit;

@musi.lang(name := "Bool")
export let Bool := Bool;

@musi.lang(name := "Nat")
export let Nat := Nat;

@musi.lang(name := "Int")
export let Int := Int;

@musi.lang(name := "Float")
export let Float := Float;

@musi.lang(name := "String")
export let String := String;

@musi.lang(name := "Range")
export let Range := Range;

@musi.lang(name := "ClosedRange")
export let ClosedRange := ClosedRange;

@musi.lang(name := "PartialRangeFrom")
export let PartialRangeFrom := PartialRangeFrom;

@musi.lang(name := "PartialRangeUpTo")
export let PartialRangeUpTo := PartialRangeUpTo;

@musi.lang(name := "PartialRangeThru")
export let PartialRangeThru := PartialRangeThru;

@musi.lang(name := "CString")
export let CString := CString;

@musi.lang(name := "CPtr")
export let CPtr := CPtr;

export opaque let Option[T] := data {
  | Some : T
  | None
};

export let some [T] (value : T) : Option[T] := .Some(value);

export let none [T] () : Option[T] := .None;

export let isSome [T] (value : Option[T]) : Bool :=
  case value of (
  | .Some(item) => 0 = 0
  | .None => 0 = 1
  );

export let isNone [T] (value : Option[T]) : Bool :=
  case value of (
  | .Some(item) => 0 = 1
  | .None => 0 = 0
  );

export let unwrapOr [T] (value : Option[T], fallback : T) : T :=
  case value of (
  | .Some(item) => item
  | .None => fallback
  );

export opaque let Abort := effect {
  let abort () : Empty;
};

export let abort () : Empty := perform Abort.abort();

export opaque let Rangeable[T] := class {
  let compare (left : T, right : T) : Int;
  let next (value : T) : Option[T];
  let prev (value : T) : Option[T];
};

export opaque let RangeBounds[T] := class {
  let defaultLowerBound () : T;
  let defaultUpperBound () : T;
};

export instance Rangeable[Int] {
  let compare (left : Int, right : Int) : Int := left - right;

  let next (value : Int) : Option[Int] := some[Int](value + 1);

  let prev (value : Int) : Option[Int] := some[Int](value - 1);
};

export instance RangeBounds[Int] {
  let defaultLowerBound () : Int := -1000000;

  let defaultUpperBound () : Int := 1000000;
};
