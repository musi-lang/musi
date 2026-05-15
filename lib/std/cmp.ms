--! Ordering and comparison helpers.

--- Ordering expect for comparisons.
export hidden let Ordering := data {
  | Less
  | Equal
  | Greater
};

--- Equality contract.
export let Eq [T] := shape {
  let eq (left : T, right : T) : Bit;
};

--- Less-than ordering value.
export let less () := .Less;

--- Equal ordering value.
export let equal () := .Equal;

--- Greater-than ordering value.
export let greater () := .Greater;

--- Function for `isLess`.
let orderingIsLess (target : Ordering) :=
  if target = .Less then 0 = 0 else 0 = 1;

--- Function for `isEqual`.
let orderingIsEqual (target : Ordering) :=
  if target = .Equal then 0 = 0 else 0 = 1;

--- Function for `isGreater`.
let orderingIsGreater (target : Ordering) :=
  if target = .Greater then 0 = 0 else 0 = 1;

--- Ordering contract.
export let Ord [T] := shape {
  let compare (left : T, right : T) : Ordering;
};

--- Generic equality with built-in equality semantics.
export let eq [T] (left : T, right : T) := left = right;

--- Generic inequality with built-in equality semantics.
export let notEq [T] (left : T, right : T) := left /= right;

--- Compare two integers.
export let compareInt (left : Int, right : Int) :=
  if left < right then less() else if left > right then greater() else equal();

@foreign(abi := .musi)
let Musi__compareFloatTotal (left : Float, right : Float) : Int;

--- Compare two floats with total ordering.
export let compareFloat (left : Float, right : Float) :=
  (
    let value := unsafe (Musi__compareFloatTotal(left, right));
    if value < 0 then less() else if value > 0 then greater() else equal();
  );

--- Compare two strings.
export let compareString (left : String, right : String) :=
  if left < right then less() else if left > right then greater() else equal();

--- Return smaller integer.
export let minInt (left : Int, right : Int) :=
  if compareInt(left, right) = .Greater then right else left;

--- Return larger integer.
export let maxInt (left : Int, right : Int) :=
  if compareInt(left, right) = .Less then right else left;

--- Clamp integer into inclusive bounds.
export let clampInt (value : Int, lowerBound : Int, upperBound : Int,) :=
  maxInt(lowerBound, minInt(value, upperBound));

--- Integer equality instance.
export let intEq := shape {
  let eq (left : Int, right : Int) := left = right;
};

--- Float equality instance.
export let floatEq := shape {
  let eq (left : Float, right : Float) := left = right;
};

--- String equality instance.
export let stringEq := shape {
  let eq (left : String, right : String) := left = right;
};

--- Bit equality instance.
export let boolEq := shape {
  let eq (left : Bit, right : Bit) := left = right;
};

--- Integer ordering instance.
export let intOrd := shape {
  let compare (left : Int, right : Int) := compareInt(left, right);
};

--- Float ordering instance.
export let floatOrd := shape {
  let compare (left : Float, right : Float) := compareFloat(left, right);
};

--- String ordering instance.
export let stringOrd := shape {
  let compare (left : String, right : String) := compareString(left, right);
};

--- Returns true when ordering is less.
export let isLess (target : Ordering) := orderingIsLess(target);

--- Returns true when ordering is less.
export let (self : Ordering).isLess () := orderingIsLess(self);

--- Returns true when ordering is equal.
export let isEqual (target : Ordering) := orderingIsEqual(target);

--- Returns true when ordering is equal.
export let (self : Ordering).isEqual () := orderingIsEqual(self);

--- Returns true when ordering is greater.
export let isGreater (target : Ordering) := orderingIsGreater(target);

--- Returns true when ordering is greater.
export let (self : Ordering).isGreater () := orderingIsGreater(self);
