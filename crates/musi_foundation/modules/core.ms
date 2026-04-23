@musi.known(name := "Type")
export let Type := Type;

@musi.known(name := "Array")
export let Array := Array;

@musi.known(name := "Any")
export let Any := Any;

@musi.known(name := "Unknown")
export let Unknown := Unknown;

@musi.known(name := "Syntax")
export let Syntax := Syntax;

@musi.known(name := "Empty")
export let Empty := Empty;

@musi.known(name := "Unit")
export let Unit := Unit;

@musi.known(name := "Bool")
export let Bool := Bool;

@musi.known(name := "Nat")
export let Nat := Nat;

@musi.known(name := "Int")
export let Int := Int;

@musi.known(name := "Int8")
export let Int8 := Int8;

@musi.known(name := "Int16")
export let Int16 := Int16;

@musi.known(name := "Int32")
export let Int32 := Int32;

@musi.known(name := "Int64")
export let Int64 := Int64;

@musi.known(name := "Nat8")
export let Nat8 := Nat8;

@musi.known(name := "Nat16")
export let Nat16 := Nat16;

@musi.known(name := "Nat32")
export let Nat32 := Nat32;

@musi.known(name := "Nat64")
export let Nat64 := Nat64;

@musi.known(name := "Float")
export let Float := Float;

@musi.known(name := "Float32")
export let Float32 := Float32;

@musi.known(name := "Float64")
export let Float64 := Float64;

@musi.known(name := "String")
export let String := String;

@musi.known(name := "Rune")
export let Rune := Rune;

@musi.known(name := "Bits")
export let Bits := Bits;

@musi.known(name := "Range")
export let Range := Range;

@musi.known(name := "Pin")
export let Pin := Pin;

@musi.known(name := "CString")
export let CString := CString;

@musi.known(name := "CPtr")
export let CPtr := CPtr;

export opaque let Option [T] := data {
  | Some(T)
  | None
};

export let someOf [T] (value : T) : Option[T] := .Some(value);

export let noneOf [T] () : Option[T] := .None;

export let isSome [T] (value : Option[T]) : Bool :=
  match value (
  | .Some(item) => 0 = 0
  | .None => 0 = 1
  );

export let isNone [T] (value : Option[T]) : Bool :=
  match value (
  | .Some(item) => 0 = 1
  | .None => 0 = 0
  );

export let unwrapOr [T] (value : Option[T], fallback : T) : T :=
  match value (
  | .Some(item) => item
  | .None => fallback
  );

export opaque let Abort := effect {
  let abort () : Empty;
};

export let abort () : Empty := ask Abort.abort();

export opaque let Rangeable[T] := shape {
  let compare (left : T, right : T) : Int;
  let next (value : T) : Option[T];
  let prev (value : T) : Option[T];
};

export opaque let RangeBounds[T] := shape {
  let defaultLowerBound () : T;
  let defaultUpperBound () : T;
};

export given Rangeable[Int] {
  let compare (left : Int, right : Int) : Int := left - right;
  let next (value : Int) : Option[Int] := someOf[Int] (value + 1);
  let prev (value : Int) : Option[Int] := someOf[Int](value - 1);
};

export given RangeBounds[Int] {
  let defaultLowerBound () : Int := - 1000000;
  let defaultUpperBound () : Int := 1000000;
};
