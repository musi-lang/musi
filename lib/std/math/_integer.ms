--! Integer math helpers.

--- Absolute integer value.
export let abs (value : Int) := if value >= 0 then value else - value;

--- Integer sign value.
export let signInt (value : Int) :=
  if value < 0 then -1 else if value > 0 then 1 else 0;

--- Maximum integer value.
export let max (left : Int, right : Int) :=
  if left >= right then left else right;

--- Minimum integer value.
export let min (left : Int, right : Int) :=
  if left <= right then left else right;

--- Clamp an integer into inclusive bounds.
export let clamp (value : Int, low : Int, high : Int) :=
  if value < low then low else if value > high then high else value;
