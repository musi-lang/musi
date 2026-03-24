/// Clamp a value between lo and hi (inclusive).
export let clamp [T] (value : T, lo : T, hi : T) : T where T : Ord :=
    (lo if value < lo | hi if value > hi | value if _);
