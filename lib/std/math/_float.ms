--! Floating-point math helpers.

--- Absolute float value.
export let absFloat (value : Float) := if value >= 0.0 then value else - value;

--- Maximum float value.
export let maxFloat (left : Float, right : Float) :=
  if left >= right then left else right;

--- Minimum float value.
export let minFloat (left : Float, right : Float) :=
  if left <= right then left else right;

--- Clamp a float into inclusive bounds.
export let clampFloat (value : Float, low : Float, high : Float) :=
  if value < low then low else if value > high then high else value;
