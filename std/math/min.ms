/// Return the smaller of two values.
export let min [T] (a : T, b : T) : T where T : Ord :=
    (a if a <= b | b if _);
