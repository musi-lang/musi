export opaque let Ordering := data {
  | Less
  | Equal
  | Greater
};

export let less () : Ordering := .Less;

export let equalOrder () : Ordering := .Equal;

export let greater () : Ordering := .Greater;

export let isLess (ordering : Ordering) : Bool :=
  match ordering (
  | .Less => 0 = 0
  | _ => 0 = 1
  );

export let isEqual (ordering : Ordering) : Bool :=
  match ordering (
  | .Equal => 0 = 0
  | _ => 0 = 1
  );

export let isGreater (ordering : Ordering) : Bool :=
  match ordering (
  | .Greater => 0 = 0
  | _ => 0 = 1
  );

export let eq[T] (left : T, right : T) : Bool := left = right;

export let notEq[T] (left : T, right : T) : Bool := left /= right;

export let compareInt (left : Int, right : Int) : Ordering :=
  match () (
  | _ if left < right => less()
  | _ if left > right => greater()
  | _ => equalOrder()
  );

export let compareFloat (left : Float, right : Float) : Ordering :=
  match () (
  | _ if left < right => less()
  | _ if left > right => greater()
  | _ => equalOrder()
  );

export let minInt (left : Int, right : Int) : Int :=
  match compareInt(left, right) (
  | .Greater => right
  | _ => left
  );

export let maxInt (left : Int, right : Int) : Int :=
  match compareInt(left, right) (
  | .Less => right
  | _ => left
  );

export let clampInt (value : Int, lowerBound : Int, upperBound : Int) : Int :=
  maxInt(lowerBound, minInt(value, upperBound));
