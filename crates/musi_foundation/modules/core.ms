@musi.builtin(name := "Type")
export let Type := Type;

@musi.builtin(name := "Array")
export let Array := Array;

@musi.builtin(name := "Any")
export let Any := Any;

@musi.builtin(name := "Unknown")
export let Unknown := Unknown;

@musi.builtin(name := "Syntax")
export let Syntax := Syntax;

@musi.builtin(name := "Empty")
export let Empty := Empty;

@musi.builtin(name := "Unit")
export let Unit := Unit;

@musi.builtin(name := "Bool")
export let Bool := Bool;

@musi.builtin(name := "Nat")
export let Nat := Nat;

@musi.builtin(name := "Int")
export let Int := Int;

@musi.builtin(name := "Int8")
export let Int8 := Int8;

@musi.builtin(name := "Int16")
export let Int16 := Int16;

@musi.builtin(name := "Int32")
export let Int32 := Int32;

@musi.builtin(name := "Int64")
export let Int64 := Int64;

@musi.builtin(name := "Nat8")
export let Nat8 := Nat8;

@musi.builtin(name := "Nat16")
export let Nat16 := Nat16;

@musi.builtin(name := "Nat32")
export let Nat32 := Nat32;

@musi.builtin(name := "Nat64")
export let Nat64 := Nat64;

@musi.builtin(name := "Float")
export let Float := Float;

@musi.builtin(name := "Float32")
export let Float32 := Float32;

@musi.builtin(name := "Float64")
export let Float64 := Float64;

@musi.builtin(name := "String")
export let String := String;

@musi.builtin(name := "Rune")
export let Rune := Rune;

@musi.builtin(name := "Bits")
export let Bits := Bits;

@musi.builtin(name := "Range")
export let Range := Range;

@musi.builtin(name := "Pin")
export let Pin := Pin;

@musi.builtin(name := "CString")
export let CString := CString;

@musi.builtin(name := "CPtr")
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

export opaque let Rangeable [T] := shape {
  let compare (left : T, right : T) : Int;
  let next (value : T) : Option[T];
  let prev (value : T) : Option[T];
};

export opaque let RangeBounds [T] := shape {
  let defaultLowerBound () : T;
  let defaultUpperBound () : T;
};

export given Rangeable[Int] {
  let compare (left : Int, right : Int) : Int := left - right;
  let next (value : Int) : Option[Int] := someOf[Int](value + 1);
  let prev (value : Int) : Option[Int] := someOf[Int](value - 1);
};

export given RangeBounds[Int] {
  let defaultLowerBound () : Int := - 1000000;
  let defaultUpperBound () : Int := 1000000;
};
