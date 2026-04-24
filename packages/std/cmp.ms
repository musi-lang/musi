--- Ordering result for comparisons.
export opaque let Ordering := data {
  | Less
  | Equal
  | Greater
};

--- Equality contract.
export let Eq [T] := shape {
  let eq (left : T, right : T) : Bool;
  law reflexive (value : T) := eq(value, value);
  law symmetric (left : T, right : T) := eq(left, right) = eq(right, left);
};

--- Less-than ordering value.
export let less () : Ordering := .Less;

--- Equal ordering value.
export let equal () : Ordering := .Equal;

--- Greater-than ordering value.
export let greater () : Ordering := .Greater;

--- Core operation for `isLess`.
let orderingIsLess (target : Ordering) : Bool :=
  match target (
  | .Less => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `isEqual`.
let orderingIsEqual (target : Ordering) : Bool :=
  match target (
  | .Equal => 0 = 0
  | _ => 0 = 1
  );

--- Core operation for `isGreater`.
let orderingIsGreater (target : Ordering) : Bool :=
  match target (
  | .Greater => 0 = 0
  | _ => 0 = 1
  );

--- Ordering contract.
export let Ord [T] := shape {
  let compare (left : T, right : T) : Ordering;
  law reflexive (value : T) := orderingIsEqual(compare(value, value));
  law lessGreaterDual (left : T, right : T) :=
    orderingIsLess(compare(left, right)) = orderingIsGreater(
      compare(right, left)
    );
  law equalSymmetric (left : T, right : T) :=
    orderingIsEqual(compare(left, right)) = orderingIsEqual(
      compare(right, left)
    );
};

--- Generic equality with built-in equality semantics.
export let eq [T] (left : T, right : T) : Bool := left = right;

--- Generic inequality with built-in equality semantics.
export let notEq [T] (left : T, right : T) : Bool := left /= right;

--- Core operation for `compareInt`.
export let compareInt (left : Int, right : Int) : Ordering :=
  match () (
  | _ if left < right => less()
  | _ if left > right => greater()
  | _ => equal()
  );

--- Core operation for `compareFloat`.
export let compareFloat (left : Float, right : Float) : Ordering :=
  match () (
  | _ if left < right => less()
  | _ if left > right => greater()
  | _ => equal()
  );

--- Core operation for `compareString`.
export let compareString (left : String, right : String) : Ordering :=
  match () (
  | _ if left < right => less()
  | _ if left > right => greater()
  | _ => equal()
  );

--- Core operation for `minInt`.
export let minInt (left : Int, right : Int) : Int :=
  match compareInt(left, right) (
  | .Greater => right
  | _ => left
  );

--- Core operation for `maxInt`.
export let maxInt (left : Int, right : Int) : Int :=
  match compareInt(left, right) (
  | .Less => right
  | _ => left
  );

--- Core operation for `clampInt`.
export let clampInt (
  value : Int,
  lowerBound : Int,
  upperBound : Int,
) : Int := maxInt(lowerBound, minInt(value, upperBound));

--- Integer equality given.
export let intEq :=
  given Eq[Int] {
  let eq (left : Int, right : Int) : Bool := left = right;
};

--- Float equality given.
export let floatEq :=
  given Eq[Float] {
  let eq (left : Float, right : Float) : Bool := left = right;
};

--- String equality given.
export let stringEq :=
  given Eq[String] {
  let eq (left : String, right : String) : Bool := left = right;
};

--- Boolean equality given.
export let boolEq :=
  given Eq[Bool] {
  let eq (left : Bool, right : Bool) : Bool := left = right;
};

--- Integer ordering given.
export let intOrd :=
  given Ord[Int] {
  let compare (left : Int, right : Int) : Ordering := compareInt(left, right);
};

--- Float ordering given.
export let floatOrd :=
  given Ord[Float] {
  let compare (left : Float, right : Float) : Ordering :=
    compareFloat(left, right);
};

--- String ordering given.
export let stringOrd :=
  given Ord[String] {
  let compare (left : String, right : String) : Ordering :=
    compareString(left, right);
};

--- Returns true when ordering is less.
export let isLess (target : Ordering) : Bool := orderingIsLess(target);

--- Returns true when ordering is less.
export let (self : Ordering).isLess () : Bool := orderingIsLess(self);

--- Returns true when ordering is equal.
export let isEqual (target : Ordering) : Bool := orderingIsEqual(target);

--- Returns true when ordering is equal.
export let (self : Ordering).isEqual () : Bool := orderingIsEqual(self);

--- Returns true when ordering is greater.
export let isGreater (target : Ordering) : Bool := orderingIsGreater(target);

--- Returns true when ordering is greater.
export let (self : Ordering).isGreater () : Bool := orderingIsGreater(self);
