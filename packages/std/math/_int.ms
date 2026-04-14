export let abs (value : Int) : Int :=
  match () (
  | _ if value >= 0 => value
  | _ => -value
  );

export let signInt (value : Int) : Int :=
  match () (
  | _ if value < 0 => -1
  | _ if value > 0 => 1
  | _ => 0
  );

export let max (left : Int, right : Int) : Int :=
  match () (
  | _ if left >= right => left
  | _ => right
  );

export let min (left : Int, right : Int) : Int :=
  match () (
  | _ if left <= right => left
  | _ => right
  );

export let clamp (value : Int, low : Int, high : Int) : Int :=
  match () (
  | _ if value < low => low
  | _ if value > high => high
  | _ => value
  );
