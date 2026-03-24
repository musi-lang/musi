/// Absolute value for integers.
export let abs (x : Int) : Int := (x if x >= 0 | -x if _);

/// Absolute value for floats.
export let abs_float (x : Float) : Float := (x if x >= 0.0 | -x if _);
