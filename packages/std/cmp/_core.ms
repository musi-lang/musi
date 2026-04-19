--- Ordering result for comparisons.
export opaque let Ordering := data {
  | Less
  | Equal
  | Greater
};

--- Equality contract.
export let Eq [T] := class {
  let eq (left : T, right : T) : Bool;
  law reflexive(value : T) := eq(value, value);
  law symmetric(left : T, right : T) := eq(left, right) = eq(right, left);
};

--- Less-than ordering value.
export let less () : Ordering := .Less;

--- Equal ordering value.
export let equal () : Ordering := .Equal;

--- Greater-than ordering value.
export let greater () : Ordering := .Greater;

--- Internal implementation for `isLess`.
export let isLess (target : Ordering) : Bool :=
  match target(
    | .Less => 0 = 0
    | _ => 0 = 1);

--- Internal implementation for `isEqual`.
export let isEqual (target : Ordering) : Bool :=
  match target(
    | .Equal => 0 = 0
    | _ => 0 = 1);

--- Internal implementation for `isGreater`.
export let isGreater (target : Ordering) : Bool :=
  match target(
    | .Greater => 0 = 0
    | _ => 0 = 1);

--- Ordering contract.
export let Ord[T] := class {
  let compare (left : T, right : T) : Ordering;
  law reflexive (value : T) := isEqual(compare(value, value));
  law lessGreaterDual(left : T, right : T) :=
    isLess(compare(left, right)) = isGreater(compare(right, left));
  law equalSymmetric(left : T, right : T) :=
    isEqual(compare(left, right)) = isEqual(compare(right, left));
};

--- Generic equality with built-in equality semantics.
export let eq[T](left : T, right : T) : Bool := left = right;

--- Generic inequality with built-in equality semantics.
export let notEq[T](left : T, right : T) : Bool := left /= right;

foreign "musi" let compareFloatTotalIntrinsic (left : Float, right : Float) : Int;

--- Internal implementation for `compareInt`.
export let compareInt (left : Int, right : Int) : Ordering :=
  match ()(
    | _ if left < right => less()
    | _ if left > right => greater()
    | _ => equal ()
  );

--- Internal implementation for `compareFloat`.
export let compareFloat (left : Float, right : Float) : Ordering :=
  (
    let order := unsafe {
    compareFloatTotalIntrinsic(left, right);
  };
  match ()(
    | _ if order < 0 => less()
    | _ if order > 0 => greater()
    | _ => equal()
  )
);

--- Internal implementation for `compareString`.
export let compareString (left : String, right : String) : Ordering :=
  match ()(
    | _ if left < right => less()
    | _ if left > right => greater()
    | _ => equal ()
  );

--- Internal implementation for `minInt`.
export let minInt (left : Int, right : Int) : Int :=
  match compareInt(left, right)(
    | .Greater => right
    | _ => left);

--- Internal implementation for `maxInt`.
export let maxInt (left : Int, right : Int) : Int :=
  match compareInt(left, right)(
    | .Less => right
    | _ => left);

--- Internal implementation for `clampInt`.
export let clampInt (
  value : Int,
  lowerBound : Int,
  upperBound : Int,
) : Int := maxInt(lowerBound, minInt(value, upperBound));

--- Integer equality instance.
export let intEq := instance Eq[Int] {
  let eq (left : Int, right : Int) : Bool := left = right;
};

--- Float equality instance.
export let floatEq := instance Eq[Float] {
  let eq (left : Float, right : Float) : Bool := left = right;
};

--- String equality instance.
export let stringEq := instance Eq[String] {
  let eq (left : String, right : String) : Bool := left = right;
};

--- Boolean equality instance.
export let boolEq := instance Eq[Bool] {
  let eq (left : Bool, right : Bool) : Bool := left = right;
};

--- Integer ordering instance.
export let intOrd := instance Ord[Int] {
  let compare (left : Int, right : Int) : Ordering := compareInt(left, right);
};

--- Float ordering instance.
export let floatOrd := instance Ord[Float] {
  let compare (left : Float, right : Float) : Ordering :=
    compareFloat(left, right);
};

--- String ordering instance.
export let stringOrd := instance Ord[String] {
  let compare (left : String, right : String) : Ordering :=
    compareString(left, right);
};
