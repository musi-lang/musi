/// Return the larger of two values.
export let max [T] (a : T, b : T) : T where T : Ord :=
    (a if a >= b | b if _);
