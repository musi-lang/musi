export let absFloat (value : Float) : Float :=
  match () (
  | _ if value >= 0.0 => value
  | _ => -value
  );

export let maxFloat (left : Float, right : Float) : Float :=
  match () (
  | _ if left >= right => left
  | _ => right
  );

export let minFloat (left : Float, right : Float) : Float :=
  match () (
  | _ if left <= right => left
  | _ => right
  );

export let clampFloat (value : Float, low : Float, high : Float) : Float :=
  match () (
  | _ if value < low => low
  | _ if value > high => high
  | _ => value
  );
