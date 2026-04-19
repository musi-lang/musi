--- Absolute integer value.
export let abs (value : Int) : Int :=
  match () (
  | _ if value >= 0 => value
  | _ => - value
  );

--- Integer sign value.
export let signInt (value : Int) : Int :=
  match () (
  | _ if value < 0 => - 1
  | _ if value > 0 => 1
  | _ => 0
  );

--- Maximum integer value.
export let max (left : Int, right : Int) : Int :=
  match () (
  | _ if left >= right => left
  | _ => right
  );

--- Minimum integer value.
export let min (left : Int, right : Int) : Int :=
  match () (
  | _ if left <= right => left
  | _ => right
  );

--- Clamp an integer into inclusive bounds.
export let clamp (value : Int, low : Int, high : Int) : Int :=
  match () (
  | _ if value < low => low
  | _ if value > high => high
  | _ => value
  );
